

import FaH.Types
import FaH.Logging
import FaH.Tool.Protomol.VMD.RMSD

import Control.Concurrent
import Data.Tagged

testlogger = let tool :: Tool ()
                 tool = addLog "hello"
             in do (l,tid,chan) <- newLogger
                   runTool tool (Tool l undefined)
                   finish chan

testrmsd = let ti = ToolInfo r c wa undefined 
               r = Tagged 1
               c = Tagged 2
               wa = Tagged "/tmp/hsfah/wa"
               fileinfo = FileInfo { vmd_bin = "vmd"
                                   , psfpath = "/tmp/hsfah/ww.psf"
                                   , foldedpath = "/tmp/hsfah/ww_folded.pdb"
                                   , scriptname = "rmsd.tcl"
                                   , resultsname = "rmsd.out"
                                   , dcdname = "ww.dcd"
                                   , atomselect = Tagged "all"
                                   , screenoutput = DevNull
                                   }
               genparams = genParams fileinfo
               remove ps = [script ps, outfile ps]
           in do (l,tid,chan) <- newLogger
                 res <- runTool (rmsd genparams remove) (Tool l ti)
                 threadDelay 100000
                 print res
