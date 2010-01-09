
module FaH.Logging where

import FaH.Types

import Control.Concurrent (ThreadId, Chan, newChan, writeChan, readChan, forkIO,)
import Control.Monad (forever)


logger :: Chan (Message String) -> IO ()
logger chan = do
  msg <- readChan chan
  case msg of
    Stop -> return ()
    Msg m -> do putStrLn m
                logger chan

logging chan str = writeChan chan (Msg str)


newLogger :: IO (Logger, ThreadId, Chan (Message String))
newLogger = do chan <- newChan
               tid <- forkIO . forever $ logger chan
               return $ (logging chan, tid, chan)

finish :: Chan (Message a) -> IO ()
finish chan = writeChan chan Stop



testl = let tool :: Tool ()
            tool = mapM_ addLog ["hello","world"]
        in do (l,tid, chan) <- newLogger
              r <- runTool tool (Tool l undefined)
              l $ show r
              writeChan chan Stop
