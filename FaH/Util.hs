
-- | Utility functions

module FaH.Util where

import FaH.Types

import Control.Concurrent (Chan, ThreadId, myThreadId, killThread, forkIO, readChan, writeChan)
import Control.Monad (forever)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c


loggerThread :: Chan (Message Log) -> IO ThreadId
loggerThread chan = forkIO . forever $ do
  msg <- readChan chan
  case msg of
    Stop        -> myThreadId >>= killThread
    Msg (Log s) -> putStrLn s


logStr :: Chan (Message Log) -> Log -> IO ()
logStr chan l = writeChan chan $ Msg l