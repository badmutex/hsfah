
module FaH.Project (doProject) where

import FaH.Types
import FaH.Logging
import FaH.Util

import Control.Applicative ((<$>))
import Control.Concurrent
import Data.Tagged
import System.Directory
import Control.Monad
import Data.Either


-- | verifies that the project data is accessible
validate :: FaHProject Unchecked -> IO (Maybe (FaHProject Checked))
validate p = let true = (== True)
                 p' = unTagged p
             in do
               pathsOK <- all true `fmap` mapM doesDirectoryExist [projectPath p']
               return $ if pathsOK then Just $ retag p else Nothing


mapTool :: Tool a -> Logger -> [ToolInfo] -> IO [Either String a]
mapTool t l tis = mapM work tis
    where work ti = runTool t (reader ti)
          reader ti = Tool l ti


-- | Filter the ToolInfos given a range of run values (low inclusive, high non-inclusive)
runRange :: (Run,Run) -> [ToolInfo] -> [ToolInfo]
runRange (rl,rh) = filter f
    where f ti = let r = unTagged . run $ ti
                     l = unTagged rl
                     h = unTagged rh
                 in l <= r && r < h



-- | Validates the project then runs it on the givin range of runs if validation is successfull.
--   The resulting eithers are paritioned into (error messages, results)
doProject :: FaHProject Unchecked -> Tool a -> (Run,Run) -> IO (Either String ([String], [a]))
doProject proj tool range = do
  checked <- validate proj
  case checked of
    Nothing -> return $ Left "Project could not be validated"
    Just p  -> do createDirectoryIfMissing True (workPath $ unTagged p)
                  (l,_,_) <- newLogger
                  res <- partitionEithers <$> mapTool tool l (runRange range $ toolInfos p)
                  removeDirectoryRecursive (workPath $ unTagged p)
                  return $ Right res