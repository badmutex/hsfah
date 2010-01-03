

module FaH.Driver where

import FaH.Tool
import FaH.Types
import FaH.Tools.ProtomolVMDRMSD
import FaH.WorkArea

import Data.Tagged

import System.FilePath


doWork :: ProjectParameters -> [Tool] -> IO WorkArea -> IO [Result]
doWork params ts genWA = apply ts . toolInfos params =<< genWA


test = let ti = mkToolInfo 808 1 (Tagged "/home/badi/Research/fah/test/data/PROJ10001") (Tagged "/tmp")
           tp = Tagged "/home/badi/Research/fah/test/data/PROJ10001/RUN808/CLONE1"
           tb = unTagged tp </> "results-000.tar.bz2"

           pps = ProjectParameters {
                   runs = 1
                 , clones = 1
                 , location = Tagged "/home/badi/Research/fah/afs-crc-fah/fahnd01/data01/data/PROJ10001"
                 }


       in doWork pps [tool] defaultWorkArea

tool :: Tool
tool ti = do res <- process ti
             print $ length `fmap` res
             return $ Right ()
