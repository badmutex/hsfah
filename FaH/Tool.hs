{-# LANGUAGE
  FlexibleInstances
  , MultiParamTypeClasses
  , TypeSynonymInstances
  #-}

module FaH.Tool where

import FaH.Types
import FaH.WorkArea

import Data.Either (partitionEithers)
import Data.Tagged
import Control.Applicative ((<$>))
import Control.Monad (mapM_, sequence_)
import System.FilePath
import Text.Printf

applyTool :: Tool -> ToolInfo -> Action
applyTool t = t

applyAllTools :: [Tool] -> ToolInfo -> [Action]
applyAllTools ts ps = map (flip applyTool ps) ts

doAllTools :: [Tool] -> ToolInfo -> IO [Result]
doAllTools ts = sequence . applyAllTools ts

doAllTrajs :: [Tool] -> [ToolInfo] -> IO [Result]
doAllTrajs ts trajs = concat <$> mapM (doAllTools ts) trajs

instance Apply  Tool  ToolInfo  Action         where apply = applyTool
instance Apply [Tool] ToolInfo [Action]        where apply = applyAllTools
instance Apply [Tool] ToolInfo   (IO [Result]) where apply = doAllTools
instance Apply [Tool] [ToolInfo] (IO [Result]) where apply = doAllTrajs
