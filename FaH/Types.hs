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
data PProjArea
data PWorkArea
data PTrajPath

-- if the types need to be changed in the database, 
-- this is the place to do so
_db_structure_id_type = "integer"
_db_run_type          = "integer"
_db_clone_type        = "integer"
_db_frame_type        = "integer"
_db_rep_type          = "integer"

-- I shouldn't be able to treat runs and clones as the same.
-- Same goes for the workarea/project paths
type Run      = Tagged PRun Integer
type Clone    = Tagged PClone Integer
type WorkArea = Tagged PWorkArea FilePath
type ProjArea = Tagged PProjArea FilePath
type TrajPath = Tagged PTrajPath FilePath

-- The info that a tool has access to
data ToolInfo = ToolInfo {
      run         :: Run
    , clone       :: Clone
    , workarea    :: WorkArea
    , projectArea :: ProjArea
    }



type Stat a = Either String a
type Status = Stat ()

type Action = IO Status

type Tool = ToolInfo -> Action

class Apply a b c where apply :: a -> b -> c

type DBTool = DB.IConnection c => c -> Tool



data ProjectParameters = ProjectParameters {
      runs :: Run
    , clones :: Clone
    , gens :: Integer
    , location :: FilePath
    }