
module Main where

import FaH.Types
import FaH.RunMapper

import FaH.Logging
import FaH.Tool.Protomol
import FaH.Tool.Protomol.VMD.RMSD

import Data.Tagged
import System.Random
import Control.Applicative ((<$>))



tester = do (l,_,chan) <- newLogger
            let runinfo = RunInfo (Tagged 0) (map Tagged [1..7]) testProjArea testWorkArea
                testProjArea = Tagged "/home/badi/Research/fah/tmp"
                testWorkArea = Tagged "/tmp/hsfah/wa"
                reader = RunMapper l runinfo theTool

            res <- runRunMapper runMapper reader
            l $ show res


randomFail :: IO Bool
randomFail = getStdRandom random


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
                res <- concat <$> protomol (rmsd genparams remove)
                addLog $ "test tool finished. length: " ++  show (length res)
                return ()
