
import FaH.Types
import FaH.Logging
import FaH.Tool.Protomol
import FaH.Tool.Protomol.VMD.RMSD

import Data.Tagged
import Control.Concurrent
import System.Directory


  


testp = let ti = ToolInfo (Tagged 808) (Tagged 1) (Tagged "/tmp/wa/") (Tagged "/home/badi/Research/fah/test/data/PROJ10000/RUN0/CLONE0")
        in do removeDirectoryRecursive "/tmp/hsfah/wa"
              createDirectory "/tmp/hsfah/wa"
              (l,_,chan) <- newLogger
              r <- runTool (protomol testrmsd) (Tool l ti)
              threadDelay 100000
              return r

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
           in do rmsd genparams remove
