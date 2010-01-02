

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


newtype DBName = DBName String           deriving Show
newtype TableName = TableName String     deriving Show
newtype TableCreate = TableCreate String deriving Show
newtype ColName = ColName String         deriving Show
newtype ColDesc = ColDesc String  deriving Show
newtype TableDesc = TableDesc String     deriving Show

_master_table =
    let pf    = printf
        id    = pf "structure_id %s unsigned not null auto_increment"  _db_structure_id_type
        run   = pf "run %s unsigned not null"                          _db_run_type
        clone = pf "clone %s unsigned not null"                        _db_clone_type
        frame = pf "frame %s unsigned not null"                        _db_frame_type
        cols  = (intercalate ", " [id, run, clone, frame])
        desc  = pf "%s, primary key ( structure_id )" cols
    in (TableName "master", TableDesc desc)



tableCreate :: TableName -> TableDesc -> TableCreate
tableCreate (TableName name) (TableDesc desc) =
    let str = printf "create table if not exists %s ( %s )" name desc
    in TableCreate str


newTable :: ColDesc -> TableName -> TableCreate
newTable (ColDesc col) =
    let master = printf "structure_id %s unsigned not null" _db_structure_id_type
        rep    = printf "rep %s unsigned not null auto_increment" _db_rep_type
        foreign = printf "foreign key (%s) references master(%s)" "structure_id" "structure_id"
        primary = printf "primary key (%s)" "structure_id, rep"
        desc = intercalate ", " [rep, master, col, foreign, primary]
    in flip tableCreate (TableDesc desc)



-- Creates the tables, does not insert anything
doCreateTables :: DB.IConnection c => [TableCreate] -> c -> IO ()
doCreateTables ts c = do
  mapM_ (\(TableCreate s) -> DB.quickQuery c s []) ts
  DB.commit c

doAddTable :: DB.IConnection c => TableCreate -> c -> IO ()
doAddTable t = doCreateTables [t]



test =  do
         c <- connectMySQL defaultMySQLConnectInfo {
                        mysqlHost = "localhost"
                      , mysqlUser = "badi"
                      , mysqlDatabase = "test"
                      , mysqlUnixSocket = "/var/run/mysqld/mysqld.sock"
                      }

         let ts = [uncurry tableCreate _master_table, newTable (ColDesc "bar float") (TableName "foo")]
         doCreateTables ts c

         DB.disconnect c