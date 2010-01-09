
module FaH.Logging where

import FaH.Types

import Control.Concurrent (Chan, newChan, writeChan, readChan)


logger :: Chan (Message String) -> IO ()
logger chan = do
  msg <- readChan chan
  case msg of
    Stop -> return ()
    Msg m -> do putStrLn m
                logger chan

logging chan str = writeChan chan (Msg str)


newLogger :: IO (Logger,IO ())
newLogger = do chan <- newChan
               return $ (logging chan, logger chan)