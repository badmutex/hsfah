

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

_master_table =
    let pf    = printf
        run   = pf "run %s unsigned not null"                          _db_run_type
        clone = pf "clone %s unsigned not null"                        _db_clone_type
        frame = pf "frame %s unsigned not null"                        _db_frame_type
        id    = pf "structure_id %s unsigned not null"  _db_structure_id_type
        cols  = (intercalate ", " [run, clone, frame, id])
        desc  = pf "%s, primary key ( run,clone,frame )" cols
    in (TableName "master", TableDesc desc)



tableCreate :: TableName -> TableDesc -> TableCreate
tableCreate (TableName name) (TableDesc desc) =
    let str = printf "create table if not exists %s ( %s )" name desc
    in TableCreate str


newTable :: ColDesc -> TableName -> TableCreate
newTable (ColDesc col) =
    let master  = printf "structure_id %s unsigned not null"       _db_structure_id_type
        rep     = printf "rep %s unsigned not null auto_increment" _db_rep_type
        foreign = printf "foreign key (%s) references master(%s)" "structure_id" "structure_id"
        primary = printf "primary key (%s)" "structure_id, rep"
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