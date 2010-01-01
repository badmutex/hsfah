{-# LANGUAGE
  EmptyDataDecls
  , RankNTypes
  #-}

module FaH.Types where

import Data.Tagged


data PRun
data PClone

type Run = Tagged PRun Integer
type Clone = Tagged PClone Integer

data Project = Project {
      runs     :: [Run]
    , clones   :: [Clone]
    , location :: FilePath
    } deriving (Eq, Show)

data ToolParameters = Params Run Clone FilePath deriving (Eq, Show)

type Action = IO

newtype Tool a = Tool (ToolParameters -> Action a)