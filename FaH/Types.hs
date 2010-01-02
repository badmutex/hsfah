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
_db_structure_id_type = "varchar(200)"
_db_run_type          = "integer unsigned not null"
_db_clone_type        = "integer unsigned not null"
_db_frame_type        = "integer unsigned not null"
_db_rep_type          = "integer unsigned not null auto_increment"

-- I shouldn't be able to treat runs and clones as the same.
-- Same goes for the workarea/project paths
type Run      = Tagged PRun Int
type Clone    = Tagged PClone Int
type Frame    = Tagged PFrame Integer
type WorkArea = Tagged PWorkArea FilePath
type ProjArea = Tagged PProjArea FilePath
type TrajPath = Tagged PTrajPath FilePath
type StructId = Tagged PStructId String


-- these are used to control the database interactions
newtype TableCreate = TableCreate String deriving Show -- ^ passed to HDBC to create the table
newtype DBName      = DBName String      deriving Show
newtype TableName   = TableName String   deriving Show
newtype ColName     = ColName String     deriving Show
newtype ColDesc     = ColDesc String     deriving Show -- ^ used in the creation of a table
newtype ColComment  = ColComment String  deriving Show
newtype TableDesc   = TableDesc String   deriving Show -- ^ 'create table <name> ( <desc> )'"

-- | Used to choose either the sql 'MAX' or 'MIN' function in 'SELECT'
data SqlOrd = Max | Min deriving Show

-- names for the database stuff.
-- ideally these would be strongly typed, but we're just hacking now.
_db_table_master       = "master"
_db_table_master_run   = "run"
_db_table_master_clone = "clone"
_db_table_master_frame = "frame"
_db_struct_id          = "structure_id"
_db_rep                = "rep"


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
      runs :: Int
    , clones :: Int
    , gens :: Integer
    , location :: FilePath
    }
