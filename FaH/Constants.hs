
-- | Global variables


module FaH.Constants where

import FaH.Types (TableName (..), TableDesc (..))

import Data.List (intercalate)
import Text.Printf (printf)


-- if the types need to be changed in the database, 
-- this is the place to do so
_db_structure_id_type = "varchar(200)"
_db_run_type          = "integer unsigned not null"
_db_clone_type        = "integer unsigned not null"
_db_frame_type        = "integer unsigned not null"
_db_rep_type          = "integer unsigned not null auto_increment"


-- names for the database stuff.
-- ideally these would be strongly typed, but we're just hacking now.
_db_table_master       = "master"
_db_table_master_run   = "run"
_db_table_master_clone = "clone"
_db_table_master_frame = "frame"
_db_struct_id          = "structure_id"
_db_rep                = "rep"


-- creation info for the master table
_master_table =
    let run   = printf "%s %s" _db_table_master_run   _db_run_type
        clone = printf "%s %s" _db_table_master_clone _db_clone_type
        frame = printf "%s %s" _db_table_master_frame _db_frame_type
        id    = printf "%s %s" _db_struct_id          _db_structure_id_type
        cols  = (intercalate ", " [run, clone, frame, id])
        desc  = printf "%s, primary key ( %s, %s, %s )" cols _db_table_master_run _db_table_master_clone _db_table_master_frame
    in (TableName "master", TableDesc desc)


_workarea_name = "_hsfah"
_default_workarea_location = "/tmp"