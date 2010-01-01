{-# LANGUAGE
  FlexibleInstances
  , MultiParamTypeClasses
  , TypeSynonymInstances
  #-}

module FaH.Tool where

import FaH.Types

import Control.Monad (mapM_, sequence_)

applyTool :: Tool -> Trajectory -> Action
applyTool t = t

applyAllTools :: [Tool] -> Trajectory -> [Action]
applyAllTools ts ps = map (flip applyTool ps) ts

doAllTools :: [Tool] -> Trajectory -> Action
doAllTools ts = sequence_ . applyAllTools ts

doAllTrajs :: [Tool] -> [Trajectory] -> Action
doAllTrajs ts trajs = mapM_ (doAllTools ts) trajs

instance Apply Tool Trajectory Action     where apply = applyTool
instance Apply [Tool] Trajectory [Action] where apply = applyAllTools
instance Apply [Tool] Trajectory Action   where apply = doAllTools
instance Apply [Tool] [Trajectory] Action where apply = doAllTrajs
