{-# LANGUAGE
  EmptyDataDecls
  #-}

module FaH.Types ( Run, Clone
                 , ProjArea, WorkArea

                 , RunVal (..), CloneVal (..)
                 , WorkAreaVal (..), ProjAreaVal (..)

                 , Message (..)
                 , Log (..)
                 , Logger

                 , ToolInfo (..)

                 , Tool

                 ) where


import Control.Concurrent (Chan)
import Control.Monad.Error (ErrorT)
import Control.Monad.State (StateT)

import Data.Tagged


data PRun
data PClone
data PProjArea
data PWorkArea

type RunType   = Int
type CloneType = Int

type Run       = Tagged PRun RunType
type Clone     = Tagged PClone CloneType
type ProjArea  = Tagged PProjArea FilePath
type WorkArea  = Tagged PWorkArea FilePath

data Message a = Stop | Msg a

newtype Log    = Log String
type Logger    = Log -> IO ()

class RunVal a where run :: a -> Run
class CloneVal a where clone :: a -> Clone
class WorkAreaVal a where workArea :: a -> WorkArea
class ProjAreaVal a where projArea :: a -> ProjArea

data ToolInfo  = ToolInfo {
      ti_run      :: Run
    , ti_clone    :: Clone
    , ti_workArea :: WorkArea
    , logger   :: Logger
    }

instance RunVal ToolInfo where run = ti_run
instance CloneVal ToolInfo where clone = ti_clone
instance WorkAreaVal ToolInfo where workArea = ti_workArea

data TrajInfo = TrajInfo {
      traj_run :: Run
    , clones :: [Clone]
    , traj_workArea :: WorkArea
    , traj_projArea :: ProjArea
    }

instance RunVal TrajInfo where run = traj_run
instance WorkAreaVal TrajInfo where workArea = traj_workArea
instance ProjAreaVal TrajInfo where projArea = traj_projArea


type Tool a = ErrorT String (StateT ToolInfo IO) a

type TrajTool a = ErrorT String (StateT (TrajInfo,Tool a) IO) a
