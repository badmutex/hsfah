
module FaH.Util where

import FaH.Types


import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Tagged

import System.FilePath ((</>))


trajPath :: ProjArea -> Run -> Clone -> TrajArea
trajPath (Tagged wa)
         (Tagged r)
         (Tagged c) = Tagged $ wa </> "RUN" ++ show r </> "CLONE" ++ show c


toolInfos :: TrajInfo -> [ToolInfo]
toolInfos (TrajInfo run clones projarea workarea) =
    map mk clones
        where mk c = ToolInfo { run = run
                              , clone = c
                              , workArea = workarea
                              , trajArea = trajPath projarea run c
                              }

ti = ToolInfo (Tagged 1) (Tagged 2) (Tagged "/tmp/wa") (Tagged "/tmp/ta")
trji = TrajInfo (Tagged 1) (map Tagged [0..5]) (Tagged "/tmp/pa") (Tagged "/tmp/wa")

testtool :: Tool Int
testtool = do
  ti <- getToolInfo
  addLog "testtool"
  return . unTagged . clone $ ti


fah :: Tool Int -> FaH Int
fah tool = do
  tri <- get
  addLog "fah"
  doTool tool
  

testfah = runFaH (fah testtool) trji ti
