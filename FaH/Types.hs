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

                 , ToolInfo (..), TrajInfo (..)
                 , ToolReader (..)

                 , Tool, Traj
                 , runTool, runTraj

                 , getToolInfo, doTool
                 , useToolInfo, getToolInfoVal, getRunVal, getCloneVal

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


data TrajInfo = TrajInfo Run [Clone] ProjArea WorkArea deriving Show

type Tool = ErrorT String (ReaderT ToolReader IO)

runTool :: Tool a -> ToolReader -> IO (Either String a)
runTool t = runReaderT (runErrorT t)


type Traj' = ErrorT String (ReaderT Logger (ReaderT TrajInfo (ReaderT (Tool ()) IO)))

runTraj' :: Traj a -> Logger -> TrajInfo -> Tool a -> IO (Either String a)
runTraj' tr l tri = runReaderT (runReaderT (runReaderT (runErrorT tr) l) tri)


type Traj a = ErrorT String (ReaderT Logger (ReaderT TrajInfo (ReaderT (Tool a) IO))) a

runTraj :: Traj a -> Logger -> TrajInfo -> Tool a -> IO (Either String a)
runTraj tr l tri = runReaderT (runReaderT (runReaderT (runErrorT tr) l) tri)



class Log m where
    addLog :: String -> m ()


instance Log Tool where
    addLog s = do l <- toolLogger `liftM` ask
                  liftIO $ l s





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


trajLogger :: Traj' Logger
trajLogger = lift $ ask

trajLog :: String -> Traj' ()
trajLog s = do l <- trajLogger
               liftIO $ l s



toolLog s = tell [s]
fahLog s = lift . lift . tell $ [s]
-- throw = fail

doTool = lift . lift
