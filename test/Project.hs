

import FaH.Types
import FaH.Exceptions
import FaH.Logging
import FaH.Util
import FaH.Project
import FaH.Tool.Protomol
import FaH.Tool.Protomol.Generation
import FaH.Tool.Protomol.CountFrames
import FaH.Tool.Protomol.VMD.RMSD

import Control.Applicative ((<$>))
import Data.Tagged
import Data.Maybe
import System.Directory
import Data.List
import Text.Printf


_fah = "/home/badi/Research/fah/afs-crc-fah/fahnd01/data01/data/PROJ10001"

proj :: FaHProject Unchecked
proj = Tagged $ Project { projectPath = _fah
                        , workPath = "/tmp/hsfah/"
                        , numRuns = 100
                        , numClones = 2
                        }

theTool :: Tool ()
theTool = let ti = ToolInfo r c wa undefined 
              r = Tagged 1
              c = Tagged 2
              wa = Tagged "/tmp/hsfah/wa"
              fileinfo = FileInfo { vmd_bin = "vmd"
                                  , psfpath = "/home/badi/Research/fah/analysis/analysis/ww_exteq_nowater1.psf"
                                  , foldedpath = "/home/badi/Research/fah/analysis/analysis/ww_folded_min.pdb"
                                  , scriptname = "rmsd.tcl"
                                  , resultsname = "rmsd.out"
                                  , dcdname = "ww.dcd"
                                  , atomselect = Tagged "all"
                                  , screenoutput = DevNull
                                  }
              genparams = genParams fileinfo
              remove ps = [script ps, outfile ps]
          in do addLog "Starting test tool"
                res <- intercalate "\n" . map show . nub . concat <$> protomol (rmsd genparams remove)
                addLog $ "test tool finished. length: " ++  show (length res)
                safeLiftIO $ appendFile "/tmp/hsfah.results" $ res ++ "\n"
                return ()

theTool2 :: FilePath -> Tool ()
theTool2 outfile = do
  gen    <- generation
  frames <- frames (CatDCD "catdcd") (DCDFile "ww.dcd")

  r      <- getRunVal
  c      <- getCloneVal


  let out = printf "%d|%d|%d|%d" r c gen frames
  safeLiftIO $ appendFile outfile (out ++ "\n")

  addLog out


test = doProject proj (protomol $ theTool2 "/tmp/genframes.txt") (Tagged 1, Tagged 2)
