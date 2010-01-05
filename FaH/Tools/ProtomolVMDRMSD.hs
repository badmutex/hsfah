{-# LANGUAGE
  EmptyDataDecls
  #-}

module FaH.Tools.ProtomolVMDRMSD where

import FaH.Archive
import FaH.Constants
import FaH.DB
import FaH.Tool
import FaH.Types

import qualified  Database.HDBC as DB (run,clone)
import Database.HDBC hiding (run, clone)

import Database.HDBC.MySQL


import Codec.Compression.BZip
import Control.Applicative ((<$>))
import Control.Concurrent.Chan
import Data.Tagged
import Data.List (intercalate, sort)
import System.Exit
import System.FilePath
import System.FilePath.Glob
import System.Posix.Files
import System.Process
import Text.Printf

import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as BS

import Prelude hiding (log)

data PScript

type Script = Tagged PScript String
newtype AtomSelect = AS String
newtype Cmd = Cmd String


-- -------------------- parameters -------------------- --
binary = "vmd"
outname = "rmsd.out"
scriptname = "rmsd.tcl"
psffile = "/afs/crc.nd.edu/user/c/cabdulwa/ww_exteq_nowater1.psf"
dcdname = "ww.dcd"
reffile = "/dscratch/izaguirr/teamSims/santanu/analysis/ww_folded_nowater1.pdb"

logfilename = "pvmdrmsd"

atomselect = AS "protein and resid 11 to 16 21 to 26 30 to 33 and name CA"
-- atomselect = AS "protein backbone and noh and resid 11 to 16 21 to 26 30 to 33"

table_name = TableName "vmd_rmsd"
col_name = ColName "rmsd"
col_desc = ColDesc "rmsd float"

toolname = "Protomol VMD RMSD Tool"
-- --------------------------------------------------- --

log :: String -> Message String
log s = Log $ printf "[%s LOG] %s" toolname s

doLog :: Chan (Message String) -> String -> IO ()
doLog chan = writeChan chan . log


rmsdScript :: FilePath -> AtomSelect -> Script
rmsdScript outfile (AS atomselect) =
    let script = intercalate "\n" $ [
                  ""
                 , "set trajid [molinfo index 0]"
                 , "set refid [molinfo index 1]"

                 , "set outfile %s"

                 , "set ref [atomselect $refid \"%s\"]"
                 , "set traj [atomselect $trajid \"%s\"]"
                 , "set n [molinfo $trajid get numframes]"

                 , "set f [open $outfile \"w\"]"
                 , "for {set i 0} { $i < $n} {incr i} {"
                 , "    $traj frame $i"
                 , "    set fit [measure fit $ref $traj]"
                 , "    $ref move $fit"
                 , "    set rmsd [measure rmsd $ref $traj]"
                 , "    puts $f \"$rmsd\""
                 , "}"
                 , "close $f"
                 ]
    in Tagged $  printf script outfile atomselect atomselect


save_script :: FilePath -> Script -> IO ()
save_script p s = writeFile p (unTagged s)

work_name :: WorkArea -> String -> FilePath
work_name wa n = unTagged wa </> n

data CmdParams = CmdParams {
      vmd, psf, dcd, ref, script, outfile :: FilePath
    }

cmd :: CmdParams -> Cmd
cmd ps = let cmd = printf "%s -dispdev text -psf %s -dcd %s -f %s < %s >/dev/null"
                        (vmd ps)       (psf ps)  (dcd ps) (ref ps) (script ps)
         in Cmd cmd

runCmd :: Cmd -> IO (CatchError ())
runCmd (Cmd p) = do h <- runCommand p
                    e <- waitForProcess h
                    return $ case e of
                               ExitSuccess   -> Right ()
                               ExitFailure c -> let s = printf "Process |%s| failed with code %d" p c
                                                in Left s


rmsdfile_parse :: FilePath -> IO [Double]
rmsdfile_parse p = map read . words <$> readFile p


get_tarballs :: TrajPath -> IO [FilePath]
get_tarballs = fmap sort . globDir1 (compile "results-???.tar.bz2") . unTagged

extract_dcd :: FilePath -> FilePath -> IO (CatchError ())
extract_dcd target tarball = do
  vals <- unpack_tarbz2 tarball
  let dcds = filter (\(p,_) -> match pat p) vals
      pat  = compile dcdname
      dcd  = snd . head $ dcds
  case dcd of
    Right bs -> do BS.writeFile target bs
                   return $ Right ()
    Left e   -> return $ Left e


manage_tarball :: Chan (Message String) -> WorkArea -> FilePath -> IO [Double]
manage_tarball chan wa tarball =
    let workfile = work_name wa
        ps = CmdParams {
               vmd     = binary
             , psf     = psffile
             , dcd     = workfile dcdname
             , ref     = reffile
             , script  = workfile scriptname
             , outfile = workfile outname
             }
    in do
      doLog chan tarball
      extract_dcd (dcd ps) tarball
      save_script (script ps) $ rmsdScript (outfile ps) atomselect
      runCmd $ cmd ps
      ret <- rmsdfile_parse (outfile ps)

      mapM_ removeLink [script ps, outfile ps, dcd ps]

      return ret


-- process :: Analyzer [Double]
process chan info = do
  tarballs <- get_tarballs $ trajPath info
  frames   <- concat <$> mapM (manage_tarball chan (workArea info)) tarballs
  return $ Right frames

-- pps = ProjectParameters {
--         runs = 0
--       , clones = 0
--       , location = Tagged "/home/badi/Research/fah/afs-crc-fah/fahnd01/data01/data/PROJ10001"
--       }
-- ti = mkToolInfo 808 1 (Tagged "/home/badi/Research/fah/test/data/PROJ10001") (Tagged "/tmp")


connection = defaultMySQLConnectInfo {
               mysqlHost = "localhost"
             , mysqlUser = "badi"
             , mysqlDatabase = "test"
             , mysqlUnixSocket = "/var/run/mysqld/mysqld.sock"
             }



-- tool :: Tool
tool chan c ti = handleSqlError $ do
              res <- process chan ti
              let ts = [ uncurry tableCreate _master_table
                       , newTable col_desc table_name]

              doCreateTables ts c
              commit c

              ret <- case res of
                       Left e -> return $ Left e
                       Right vs  -> let structs = [(run ti, clone ti, Tagged i) | i <- [0..fromIntegral $ length vs]]
                                    in do
                                      insert c table_name col_name structs vs
                                      return $ Right ()


              -- disconnect c
              return ret
