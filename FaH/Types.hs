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

type Run = Tagged PRun Integer
type Clone = Tagged PClone Integer

data TrajectoryLocation = TrajLoc Run Clone FilePath

data Project = Project {
      trajectories :: [Trajectory]
    } deriving (Eq, Show)

data Trajectory = Trajectory {
      run            :: Run
    , clone          :: Clone
    , location :: FilePath
    } deriving (Eq, Show)


type Stat a = Either String a
type Status = Stat ()

type Action = IO Status

type Tool = TrajectoryLocation -> Action

class Apply a b c where apply :: a -> b -> c

type DBTool = DB.IConnection c => c -> Tool

