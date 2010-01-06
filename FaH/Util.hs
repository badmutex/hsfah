
module FaH.Util where

import FaH.Types


import Control.Concurrent
import Control.Monad



logger :: Chan (Message Log) -> IO ()
logger chan = forever $ do
                msg <- readChan chan
                case msg of
                  Stop        -> myThreadId >>= killThread
                  Msg (Log l) -> putStrLn l
