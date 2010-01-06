{-# LANGUAGE
  EmptyDataDecls
  #-}

module FaH.Tool.Protomol.VMD.RMSD where


import FaH.Types

import Control.Monad.Error
import Control.Monad.State

import Data.Tagged
import Data.List (intercalate)
import Text.Printf

import System.Process
import System.Exit
import System.FilePath
import System.Posix.Files

import Prelude hiding (log)


data PAtomSelect
data PScript
data PCmd

type AtomSelect = Tagged PAtomSelect String
type Script = Tagged PScript String
type Cmd = Tagged PCmd String


data CmdParams = CmdParams {
      vmd, psf, dcd, ref, script, outfile :: FilePath
    } deriving Show


save_script :: FilePath -> Script -> IO ()
save_script p s = writeFile p (unTagged s)


mkCmd :: CmdParams -> Cmd
mkCmd p = let cmd = printf "%s -dispdev text -psf %s -dcd %s -f %s < %s"
                    (vmd p) (psf p) (dcd p) (ref p) (script p)
          in Tagged cmd



runCmd :: Cmd -> IO ExitCode
runCmd cmd = do h <- runCommand $ unTagged cmd
                waitForProcess h

rmsd_results :: FilePath -> IO [Double]
rmsd_results p = (map read . words) `fmap` readFile p


-- ---------------------------------------- --
_vmd_bin = "vmd"
_psffile = "/tmp/ww.psf"
_dcdname = "/tmp/ww.dcd"
_reffile = "/tmp/ww_folded.pdb"
_rmsdname = "rmsd.tcl"
_rmsdfile = "rmsd.out"

rmsdScript :: FilePath -> AtomSelect -> Script
rmsdScript outfile atomselect =
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
    in Tagged $ printf script outfile (unTagged atomselect) (unTagged atomselect)
-- ---------------------------------------- --

rmsd :: Tool [Double]
rmsd = do
  info <- get

  let params = CmdParams {
                 vmd = _vmd_bin
               , psf = _psffile
               , dcd = wa </> _dcdname
               , ref = _reffile
               , script = wa </> _rmsdname
               , outfile = wa </> _rmsdfile
               }
      wa = unTagged $ workArea info
      rmsd_tcl = wa </> _rmsdname
      rmsdfile = wa </> _rmsdfile
      cmd = mkCmd params
      script = rmsdScript rmsdfile (Tagged "all")

      log = liftIO . putStrLn -- logger info . Log

  log $ unTagged cmd
  log $ unTagged script

  liftIO . save_script rmsd_tcl . rmsdScript rmsdfile $ Tagged "all"
  liftIO . runCmd . mkCmd $ params
  results <- liftIO $ rmsd_results rmsdfile

  -- cleanup
  liftIO $ mapM_ removeLink [rmsd_tcl,rmsdfile]

  return results


test = let ti = ToolInfo (Tagged 1) (Tagged 2) (Tagged "/tmp") undefined
       in evalStateT (runErrorT rmsd) ti