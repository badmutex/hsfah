{-# LANGUAGE
  EmptyDataDecls
  , NoMonomorphismRestriction
  , MultiParamTypeClasses
  , TypeSynonymInstances
  , ExistentialQuantification
  , RankNTypes
  , FlexibleContexts
  #-}

module FaH.Types ( Run, Clone
                 , ProjArea, TrajArea, WorkArea

                 , Message (..)
                 , Log (..)
                 , Logger

                 , ToolInfo (..), RunInfo (..)
                 , ToolReader (..), RunMapperReader (..)
                 , FaHProject, Project (..)
                 , Checked, Unchecked

                 , Tool, RunMapper
                 , runTool, runRunMapper

                 , getToolInfo, useToolInfo
                 , getToolInfoVal, getRunVal, getCloneVal
                 , getRunInfo, getTool

                 ) where


import Control.Applicative ((<$>))
import Control.Concurrent

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.List
import Control.Monad.Identity

import Data.Tagged

data PRun
data PClone
data PProjArea
data PWorkArea
data PTrajArea

type RunType   = Int
type CloneType = Int

type Run       = Tagged PRun RunType
type Clone     = Tagged PClone CloneType
type ProjArea  = Tagged PProjArea FilePath
type WorkArea  = Tagged PWorkArea FilePath
type TrajArea  = Tagged PTrajArea FilePath

type Logger    = String -> IO ()

data Message a = Stop | Msg a

data ToolInfo  = ToolInfo {
      run      :: Run
    , clone    :: Clone
    , workArea :: WorkArea
    , trajArea :: TrajArea
    } deriving Show

data ToolReader = Tool {
      toolLogger :: Logger
    , toolInfo :: ToolInfo
    }


data RunInfo = RunInfo Run [Clone] ProjArea WorkArea deriving Show


data RunMapperReader = RunMapper {
      runLogger :: Logger
    , runInfo :: RunInfo
    , tool :: Tool ()
    }

type Tool = ErrorT String (ReaderT ToolReader IO)

runTool :: Tool a -> ToolReader -> IO (Either String a)
runTool t = runReaderT (runErrorT t)


type RunMapper = ErrorT String (ReaderT RunMapperReader IO)

runRunMapper :: RunMapper a -> RunMapperReader -> IO (Either String a)
runRunMapper tr = runReaderT (runErrorT tr)


data Checked
data Unchecked

type FaHProject a = Tagged a Project

data Project = Project {
      projectPath :: FilePath
    , workPath :: FilePath
    , numRuns :: RunType
    , numClones :: CloneType
    }



class Log m where
    addLog :: String -> m ()



askAddLog f s = do l <- f <$> ask
                   liftIO $ l s

instance Log Tool where
    addLog s = askAddLog toolLogger s

instance Log RunMapper where
    addLog s = askAddLog runLogger s




getToolInfo :: Tool ToolInfo
getToolInfo = toolInfo `liftM` ask

useToolInfo :: (ToolInfo -> ToolInfo) -> Tool a -> Tool a
useToolInfo delta = local (\tr -> tr { toolInfo = delta (toolInfo tr) })

getToolInfoVal :: (ToolInfo -> Tagged a b) -> Tool b
getToolInfoVal f = unTagged . f <$> getToolInfo

getRunVal :: Tool RunType
getRunVal = getToolInfoVal run

getCloneVal :: Tool CloneType
getCloneVal = getToolInfoVal clone


getRunInfo :: RunMapper RunInfo
getRunInfo = runInfo <$> ask

getTool :: RunMapper (Tool ())
getTool = tool <$> ask


