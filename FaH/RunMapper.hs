
module FaH.RunMapper where

import FaH.Types
import FaH.Exceptions
import FaH.Util

import Control.Applicative ((<$>))
import Control.Monad.Reader



runMapper :: RunMapper [Either String ()]
runMapper = do
  tool    <- getTool
  readers <- getReaders
  mapM (safeLiftIO . runTool tool) readers



getReaders :: RunMapper [ToolReader]
getReaders = do
  logger <- runLogger <$> ask
  infos  <- toolInfos <$> getRunInfo
  return $ map (Tool logger) infos
