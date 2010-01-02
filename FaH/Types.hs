{-# LANGUAGE
  EmptyDataDecls
  , FlexibleInstances
  , MultiParamTypeClasses
  , Rank2Types
  #-}

module FaH.Types where

import Data.Convertible
import Data.Tagged

import qualified  Database.HDBC as DB


data PRun
data PClone
data PFrame
data PProjArea
data PWorkArea
data PTrajPath
data PStructId

-- if the types need to be changed in the database, 
-- this is the place to do so
_db_structure_id_type = "text"
_db_run_type          = "integer"
_db_clone_type        = "integer"
_db_frame_type        = "integer"
_db_rep_type          = "integer"

-- I shouldn't be able to treat runs and clones as the same.
-- Same goes for the workarea/project paths
type Run      = Tagged PRun Integer
type Clone    = Tagged PClone Integer
type Frame    = Tagged PFrame Integer
type WorkArea = Tagged PWorkArea FilePath
type ProjArea = Tagged PProjArea FilePath
type TrajPath = Tagged PTrajPath FilePath
type StructId = Tagged PStructId String

instance Convertible b c => Convertible (Tagged a b) c where
    safeConvert = safeConvert . unTagged

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
