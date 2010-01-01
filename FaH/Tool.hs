{-# LANGUAGE
  FlexibleInstances
  , MultiParamTypeClasses
  , TypeSynonymInstances
  #-}

module FaH.Tool where

import FaH.Types

import Control.Monad (mapM_, sequence_)

applyTool :: Tool -> TrajectoryLocation -> Action
applyTool t = t

applyAllTools :: [Tool] -> TrajectoryLocation -> [Action]
applyAllTools ts ps = map (flip applyTool ps) ts

doAllTools :: [Tool] -> TrajectoryLocation -> Action
doAllTools ts = sequence_ . applyAllTools ts

doAllTrajs :: [Tool] -> [TrajectoryLocation] -> Action
doAllTrajs ts trajs = mapM_ (doAllTools ts) trajs

instance Apply Tool TrajectoryLocation Action     where apply = applyTool
instance Apply [Tool] TrajectoryLocation [Action] where apply = applyAllTools
instance Apply [Tool] TrajectoryLocation Action   where apply = doAllTools
instance Apply [Tool] [TrajectoryLocation] Action where apply = doAllTrajs



mkTrajectory :: Run -> Clone -> FilePath -> Trajectory
mkTrajectory r c p = Trajectory { run = r, clone = c, location = p }

mkProject :: [Trajectory] -> Project
mkProject = Project

