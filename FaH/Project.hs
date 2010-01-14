
module FaH.Project where

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
               pathsOK <- all true `fmap` mapM doesDirectoryExist [projectPath p', workPath p']
               return $ if pathsOK then Just $ retag p else Nothing


mapTool :: Tool a -> Logger -> [ToolInfo] -> IO [Either String a]
mapTool t l tis = mapM work tis
    where work ti = runTool t (reader ti)
          reader ti = Tool l ti



doProject :: FaHProject Unchecked -> Tool a -> IO (Either String ([String], [a]))
doProject proj tool = do checked <- validate proj
                         case checked of
                           Nothing -> return $ Left "Project could not be validated"
                           Just p  -> do (l,_,_) <- newLogger
                                         res <- partitionEithers <$> mapTool tool l (toolInfos p)
                                         return $ Right res