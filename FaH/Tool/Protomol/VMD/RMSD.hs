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


_name = "FaH.Tool.Protomol.VMD.RMSD"


data CmdParams = CmdParams {
      vmd, psf, dcd, ref, script, outfile :: FilePath
    , screenout :: String
    } deriving Show

data Output = DevNull | Err2Out | Out2Err | Default

save_script :: FilePath -> Script -> IO ()
save_script p s = writeFile p (unTagged s)


mkCmd :: CmdParams -> Cmd
mkCmd p = let cmd = printf "%s -dispdev text -psf %s -dcd %s -f %s < %s %s"
                    (vmd p) (psf p) (dcd p) (ref p) (script p) (screenout p)
          in Tagged cmd



runCmd :: Cmd -> IO ExitCode
runCmd cmd = do h <- runCommand $ unTagged cmd
                waitForProcess h

rmsd_results :: FilePath -> IO [Double]
rmsd_results p = (map read . words) `fmap` readFile p

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


data FileInfo = FileInfo {
      vmd_bin , psfpath, foldedpath :: FilePath
    , scriptname, resultsname, dcdname :: String
    , atomselect :: AtomSelect
    , screenoutput :: Output
    }

genParams :: FileInfo -> WorkArea -> (CmdParams,AtomSelect)
genParams fi wa = (params, atomselect fi)
    where wa' = unTagged wa
          params = CmdParams {
                     vmd     = vmd_bin fi
                   , psf     = psfpath fi
                   , dcd     = wa' </> dcdname fi
                   , ref     = foldedpath fi
                   , script  = wa' </> scriptname fi
                   , outfile = wa' </> resultsname fi
                   , screenout = case screenoutput fi of
                                   DevNull -> ">/dev/null"
                                   Err2Out -> "2>&1"
                                   Out2Err -> "1>&2"
                                   Default -> ""
                   }

type GenCmdParams = WorkArea -> (CmdParams,AtomSelect)
type ChooseRemovableFiles = CmdParams -> [FilePath]



addLog' = addLog . printf "[%s] %s" _name 

-- ======================================== --

rmsd :: GenCmdParams -> ChooseRemovableFiles -> Tool [Double]
rmsd genParams removableFiles = do
  addLog' "starting"

  info <- getToolInfo

  let (params,atomsel) = genParams $ workArea info
      cmd = mkCmd params



  liftIO $ save_script (script params) 
         $ rmsdScript  (outfile params) atomsel
  liftIO $ runCmd . mkCmd
         $ params

  results <- liftIO $ rmsd_results (outfile params)

  liftIO . mapM_ removeLink $ removableFiles params

  addLog' "sucess!"
  return results

-- ======================================== --




-- ---------------------------------------- --
testrmsd = let ti = ToolInfo r c wa undefined 
               r = Tagged 1
               c = Tagged 2
               wa = Tagged "/tmp/test"
               fileinfo = FileInfo { vmd_bin = "vmd"
                                   , psfpath = "/tmp/test/ww.psf"
                                   , foldedpath = "/tmp/test/ww_folded.pdb"
                                   , scriptname = "rmsd.tcl"
                                   , resultsname = "rmsd.out"
                                   , dcdname = "ww.dcd"
                                   , atomselect = Tagged "all"
                                   , screenoutput = DevNull
                                   }
               genparams = genParams fileinfo
               remove ps = [script ps, outfile ps]
           in runTool (rmsd genparams remove) ti
-- ---------------------------------------- --
