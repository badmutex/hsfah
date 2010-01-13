{-# LANGUAGE
  NoMonomorphismRestriction
  #-}

module FaH.Exceptions ( safeLiftIO
                      , liftExitCode
                      , testFail
                      ) where


import FaH.Types

import Control.Exception
import Control.Monad.Trans

import System.Exit
import qualified System.IO.Error as IO

import Text.Printf


safeLiftIO = liftFail . liftIO . wrapIO


liftExitCode :: MonadIO m => IO ExitCode -> m ExitCode
liftExitCode io = do code <- safeLiftIO io
                     case code of
                       ExitFailure c -> fail $ printf "failed with %d" c
                       otherwise     -> return code

testFail (ExitFailure c) = True
testFail c = False


justIO :: IOException -> Maybe String
justIO e@(_) = Just $ show e

wrapIO :: IO a -> IO (Either String a)
wrapIO = tryJust justIO

liftFail :: MonadIO m => m (Either String a) -> m a
liftFail m  = m >>= lifter
    where lifter (Left msg) = fail msg
          lifter (Right v)  = return v
