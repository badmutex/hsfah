{-# LANGUAGE
  EmptyDataDecls
  , MultiParamTypeClasses
  #-}

module FaH.Types where

import Data.Tagged


data PRun
data PClone

type Run = Tagged PRun Integer
type Clone = Tagged PClone Integer

class Location a where location :: a -> FilePath

data Project = Project {
      runs     :: [Run]
    , clones   :: [Clone]
    , _proj_location :: FilePath
    } deriving (Eq, Show)

data Trajectory = Trajectory {
      run            :: Run
    , clone          :: Clone
    , _traj_location :: FilePath
    }

instance Location Project    where location = _proj_location
instance Location Trajectory where location = _traj_location


type Action = IO ()

type Tool = Trajectory -> Action


class Apply a b c where apply :: a -> b -> c
