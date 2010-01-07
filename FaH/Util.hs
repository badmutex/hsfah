
module FaH.Util where

import FaH.Types


import Control.Concurrent
import Control.Monad

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