{-# LANGUAGE
  FlexibleInstances
  , MultiParamTypeClasses
  , TypeSynonymInstances
  #-}

module FaH.Tool where

import FaH.Types

import Control.Applicative ((<$>))
import Control.Monad (mapM_, sequence_)

applyTool :: Tool -> TrajectoryLocation -> Action
applyTool t = t

applyAllTools :: [Tool] -> TrajectoryLocation -> [Action]
applyAllTools ts ps = map (flip applyTool ps) ts

doAllTools :: [Tool] -> TrajectoryLocation -> IO [Status]
doAllTools ts = sequence . applyAllTools ts

doAllTrajs :: [Tool] -> [TrajectoryLocation] -> IO [Status]
doAllTrajs ts trajs = concat <$> mapM (doAllTools ts) trajs

instance Apply Tool TrajectoryLocation Action            where apply = applyTool
instance Apply [Tool] TrajectoryLocation [Action]        where apply = applyAllTools
instance Apply [Tool] TrajectoryLocation (IO [Status])   where apply = doAllTools
instance Apply [Tool] [TrajectoryLocation] (IO [Status]) where apply = doAllTrajs



mkTrajectory :: Run -> Clone -> FilePath -> Trajectory
mkTrajectory r c p = Trajectory { run = r, clone = c, location = p }

mkProject :: [Trajectory] -> Project
mkProject = Project

