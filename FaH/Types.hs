{-# LANGUAGE
  EmptyDataDecls
  , MultiParamTypeClasses
  , Rank2Types
  #-}

module FaH.Types where

import Data.Tagged

import qualified  Database.HDBC as DB


data PRun
data PClone

_db_structure_id_type = "integer"
_db_run_type = "integer"
_db_clone_type = "integer"
_db_frame_type = "integer"
_db_rep_type = "integer"

type Run = Tagged PRun Integer
type Clone = Tagged PClone Integer

data TrajectoryLocation = TrajLoc Run Clone FilePath

data Project = Project {
      trajectories :: [Trajectory]
    } deriving (Eq, Show)

data Trajectory = Trajectory {
      run            :: Run
    , clone          :: Clone
    , _location :: FilePath
    } deriving (Eq, Show)


type Stat a = Either String a
type Status = Stat ()

type Action = IO Status

type Tool = TrajectoryLocation -> Action

class Apply a b c where apply :: a -> b -> c

type DBTool = DB.IConnection c => c -> Tool



data ProjectParameters = ProjectParameters {
      runs :: Run
    , clones :: Clone
    , gens :: Integer
    , location :: FilePath
    }