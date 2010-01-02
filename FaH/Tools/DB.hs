

-- | (run, clone, frame) identifies a single structure.
--  In a perfect world this is all we would need, but in many cases we need
--  to handle redundant data as well. This is indicated by 'rep' (for replication.
--  This way we can just put all the data in the DB and worry about the filtration later.
--
--  So: (run, clone, frame) maps to a single structure, which 'rep' indicating redundant frames.
--  If 'rep' = 0 then the frame is not present
--
--  Tools can further specify a column to associate with a frame, ie RMSD, or contacts, etc.

module FaH.Tools.DB where

import FaH.Types

import Control.Applicative ((<$>))
import Data.List (intercalate)
import qualified  Database.HDBC as DB
import Database.HDBC.MySQL
import Text.Printf


newtype TableCreate = TableCreate String deriving Show -- ^ passed to HDBC to create the table
newtype DBName      = DBName String      deriving Show
newtype TableName   = TableName String   deriving Show
newtype ColName     = ColName String     deriving Show
newtype ColDesc     = ColDesc String     deriving Show -- ^ used in the creation of a table
newtype TableDesc   = TableDesc String   deriving Show -- ^ 'create table <name> ( <desc> )'"

data SqlOrd = Max | Min deriving Show

-- column names for the tables.
_db_table_master_run   = "run"
_db_table_master_clone = "clone"
_db_table_master_frame = "frame"
_db_struct_id          = "structure_id"
_db_rep                = "rep"

_master_table =
    let run   = printf "%s %s unsigned not null" _db_table_master_run   _db_run_type
        clone = printf "%s %s unsigned not null" _db_table_master_clone _db_clone_type
        frame = printf "%s %s unsigned not null" _db_table_master_frame _db_frame_type
        id    = printf "%s %s unsigned not null" _db_struct_id          _db_structure_id_type
        cols  = (intercalate ", " [run, clone, frame, id])
        desc  = printf "%s, primary key ( %s, %s, %s )" cols _db_table_master_run _db_table_master_clone _db_table_master_frame
    in (TableName "master", TableDesc desc)



tableCreate :: TableName -> TableDesc -> TableCreate
tableCreate (TableName name) (TableDesc desc) =
    let str = printf "create table if not exists %s ( %s )" name desc
    in TableCreate str


newTable :: ColDesc -> TableName -> TableCreate
newTable (ColDesc col) =
    let master  = printf "%s %s unsigned not null"                _db_struct_id _db_structure_id_type
        rep     = printf "%s %s unsigned not null auto_increment" _db_rep       _db_rep_type
        foreign = printf "foreign key (%s) references master(%s)" _db_struct_id _db_struct_id
        keys    = printf "%s, %s"                                 _db_struct_id _db_rep
        primary = printf "primary key (%s)"                       (keys :: String)
        desc = intercalate ", " [rep, master, col, foreign, primary]
    in flip tableCreate (TableDesc desc)


ordColumn :: DB.IConnection c => TableName -> ColName -> c -> SqlOrd -> IO DB.SqlValue
ordColumn (TableName tn) (ColName cn) conn ord =
    let q = printf "select %s(%s) from %s" (show ord) cn tn
    in do head . head <$>  DB.quickQuery conn q []

-- insertIntoMaster :: DB.IConnection c => [(Run, Column, Frame)] -> c -> IO ()
-- insertIntoMaster vals c = do
--   max


-- Creates the tables, does not insert anything
doCreateTables :: DB.IConnection c => [TableCreate] -> c -> IO ()
doCreateTables ts c = do
  mapM_ (\(TableCreate s) -> DB.quickQuery c s []) ts
  DB.commit c

doAddTable :: DB.IConnection c => TableCreate -> c -> IO ()
doAddTable t = doCreateTables [t]








test = do
  c <- connectMySQL defaultMySQLConnectInfo {
         mysqlHost = "localhost"
       , mysqlUser = "badi"
       , mysqlDatabase = "test"
       , mysqlUnixSocket = "/var/run/mysqld/mysqld.sock"
       }

  let ts = [uncurry tableCreate _master_table, newTable (ColDesc "bar float") (TableName "foo")]
  doCreateTables ts c

  DB.disconnect c