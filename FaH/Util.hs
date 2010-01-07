
module FaH.Util where

import FaH.Types


import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Tagged

import System.FilePath ((</>))



logger :: Chan (Message Log) -> IO ()
logger chan = forever $ do
                msg <- readChan chan
                case msg of
                  Stop        -> myThreadId >>= killThread
                  Msg (Log l) -> putStrLn l

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

testtool :: Tool ()
testtool = do
  ti <- ask
  tell ["testtool " ++ show (clone ti)]


fah :: Tool () -> FaH ()
fah tool = do
  tri <- get
  lift . lift $ tool
  return ()

testfah = runFaH (fah testtool) trji ti