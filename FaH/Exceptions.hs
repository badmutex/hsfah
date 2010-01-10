{-# LANGUAGE
  NoMonomorphismRestriction
  #-}

module FaH.Exceptions ( safeLiftIO
                      , liftExitCode
                      ) where


import FaH.Types
import FaH.Logging

import Control.Exception
import Control.Concurrent

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Error

import System.Exit
import qualified System.IO.Error as IO

import Text.Printf


safeLiftIO = liftFail . liftIO . wrapIO


liftExitCode :: MonadIO m => IO ExitCode -> m ExitCode
liftExitCode io = do code <- safeLiftIO io
                     case code of
                       ExitFailure c -> fail $ printf "failed with %d" c
                       otherwise     -> return code


justIO :: IOException -> Maybe String
justIO e@(_) = Just $ show e

wrapIO :: IO a -> IO (Either String a)
wrapIO = tryJust justIO

liftFail :: MonadIO m => m (Either String a) -> m a
liftFail m  = m >>= lifter
    where lifter (Left msg) = fail msg
          lifter (Right v)  = return v






bad = readFile "/tmp/foo"
ok  = readFile "/tmp/ok"

test :: IO (Either String String)
test = tryJust justIO
               (readFile "/tmp/ok")

tool :: Tool String
tool = do addLog "running"
          safeLiftIO bad



testf = do (l,_,chan) <- newLogger
           r <- runTool tool (Tool l undefined)
           threadDelay 100000
           print r
           finish chan

-- ErrorT String (ReaderT ToolReader IO) String
-- ErrorT String (ReaderT ToolReader IO) (Either String String)
