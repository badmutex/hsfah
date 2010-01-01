{-# LANGUAGE
  ExistentialQuantification
  #-}

module FaH.Tool where

import FaH.Types

import Control.Monad

applyTool :: Tool a -> ToolParameters -> Action a
applyTool (Tool f) = f

applyTools :: forall a. [Tool a] -> ToolParameters -> [Action a]
applyTools ts ps = map (flip applyTool ps) ts

doTools :: forall a. [Tool a] -> ToolParameters -> Action ()
doTools ts = sequence_ . applyTools ts
