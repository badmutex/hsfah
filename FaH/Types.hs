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

                 , ToolInfo (..)
                 , ToolReader (..)
                 , FaHProject, Project (..)
                 , Checked, Unchecked

                 , Tool
                 , runTool

                 , getToolInfo, useToolInfo
                 , getToolInfoVal, getRunVal, getCloneVal

                 , module Data.Tagged
                 , module Control.Applicative

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



type Tool = ErrorT String (ReaderT ToolReader IO)

runTool :: Tool a -> ToolReader -> IO (Either String a)
runTool t = runReaderT (runErrorT t)



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
