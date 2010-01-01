{-# LANGUAGE
  EmptyDataDecls
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


type Action = IO ()

type Tool = Project -> Action
