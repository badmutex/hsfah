{-# LANGUAGE
  FlexibleInstances
  , MultiParamTypeClasses
  , TypeSynonymInstances
  #-}

module FaH.Tool where

import FaH.Types

import Control.Applicative ((<$>))
import Control.Monad (mapM_, sequence_)

applyTool :: Tool -> ToolInfo -> Action
applyTool t = t

applyAllTools :: [Tool] -> ToolInfo -> [Action]
applyAllTools ts ps = map (flip applyTool ps) ts

doAllTools :: [Tool] -> ToolInfo -> IO [Status]
doAllTools ts = sequence . applyAllTools ts

doAllTrajs :: [Tool] -> [ToolInfo] -> IO [Status]
doAllTrajs ts trajs = concat <$> mapM (doAllTools ts) trajs

instance Apply Tool ToolInfo Action            where apply = applyTool
instance Apply [Tool] ToolInfo [Action]        where apply = applyAllTools
instance Apply [Tool] ToolInfo (IO [Status])   where apply = doAllTools
instance Apply [Tool] [ToolInfo] (IO [Status]) where apply = doAllTrajs
