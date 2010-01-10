
module FaH.RunMapper where

import FaH.Types
import FaH.Exceptions
import FaH.Util
import FaH.Logging


import FaH.Tool.Protomol
import FaH.Tool.Protomol.VMD.RMSD



import Control.Applicative ((<$>))
import Control.Monad.Reader
import Data.Tagged



runMapper :: RunMapper [Either String ()]
runMapper = do
  tool    <- getTool
  readers <- getReaders
  mapM (safeLiftIO . runTool tool) readers



getReaders :: RunMapper [ToolReader]
getReaders = do
  logger <- runLogger <$> ask
  infos  <- toolInfos <$> getRunInfo
  return $ map (Tool logger) infos




tester = do (l,_,chan) <- newLogger
            let runinfo = RunInfo (Tagged 0) (map Tagged [0..7]) testProjArea testWorkArea
                testProjArea = Tagged "/home/badi/Research/fah/tmp"
                testWorkArea = Tagged "/tmp/hsfah/wa"
                reader = RunMapper l runinfo theTool
            
            res <- runRunMapper runMapper reader
            l $ show res


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
