{-# LANGUAGE
  FlexibleContexts
  #-}

-- | (run, clone, frame) identifies a single structure.
--  In a perfect world this is all we would need, but in many cases we need
--  to handle redundant data as well. This is indicated by 'rep' (for replication.
--  This way we can just put all the data in the DB and worry about the filtration later.
--
--  So: (run, clone, frame) maps to a single structure, which 'rep' indicating redundant frames.
--  If 'rep' = 0 then the frame is not present
--
--  Tools can further specify a column to associate with a frame, ie RMSD, or contacts, etc.

module FaH.DB where

import FaH.Tool
import FaH.Types

import Control.Applicative ((<$>))
import Data.Convertible
import Data.List (intercalate)
import Data.STRef
import Data.Tagged
import Text.Printf

import qualified  Database.HDBC as DB (run,clone)
import Database.HDBC hiding (run, clone)

import Database.HDBC.MySQL



newtype TableCreate = TableCreate String deriving Show -- ^ passed to HDBC to create the table
newtype DBName      = DBName String      deriving Show
newtype TableName   = TableName String   deriving Show
newtype ColName     = ColName String     deriving Show
newtype ColDesc     = ColDesc String     deriving Show -- ^ used in the creation of a table
newtype TableDesc   = TableDesc String   deriving Show -- ^ 'create table <name> ( <desc> )'"

data SqlOrd = Max | Min deriving Show

-- names for the database stuff.
_db_table_master       = "master"
_db_table_master_run   = "run"
_db_table_master_clone = "clone"
_db_table_master_frame = "frame"
_db_struct_id          = "structure_id"
_db_rep                = "rep"

_master_table =
    let run   = printf "%s %s unsigned not null" _db_table_master_run   _db_run_type
        clone = printf "%s %s unsigned not null" _db_table_master_clone _db_clone_type
        frame = printf "%s %s unsigned not null" _db_table_master_frame _db_frame_type
        id    = printf "%s %s"                   _db_struct_id          _db_structure_id_type
        cols  = (intercalate ", " [run, clone, frame, id])
        desc  = printf "%s, primary key ( %s, %s, %s )" cols _db_table_master_run _db_table_master_clone _db_table_master_frame
    in (TableName "master", TableDesc desc)


mkStructId :: Run -> Clone -> Frame -> StructId
mkStructId r c f = Tagged $
                   printf "r%dc%df%d"
                   (unTagged r)
                   (unTagged c)
                   (unTagged f)


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


-- | Select the min or max value from a column
ordColumn :: IConnection c => c -> TableName -> ColName -> SqlOrd -> IO SqlValue
ordColumn conn (TableName tn) (ColName cn) ord =
    let q = printf "select %s(%s) from %s" (show ord) cn tn
    in do head . head <$>  quickQuery' conn q []

-- | Count the number of rows in a table
countRows :: IConnection c => c -> TableName -> IO Integer
countRows conn (TableName n) =
    let q = printf "select count(*) from %s" n
    in fromSql . head . head <$> quickQuery' conn q []



masterInsertVals :: [(Run, Clone, Frame)] -> [[SqlValue]]
masterInsertVals vals = map (\v -> to v (uncurry3 mkStructId v)) vals
    where to (r,c,f) i = let r' = toSql r
                             c' = toSql c
                             f' = toSql f
                             i' = toSql i
                         in [r',c',f', i']



insertIntoMaster :: IConnection c => [(Run, Clone, Frame)] -> c -> IO ()
insertIntoMaster vals c =
    let s = printf
            "insert into %s (%s,%s,%s,%s) values (?,?,?,?)"
            _db_table_master
            _db_table_master_run
            _db_table_master_clone
            _db_table_master_frame
            _db_struct_id
            :: String
        vs = masterInsertVals vals
    in do
      ps      <- prepare c s
      executeMany ps vs




-- Creates the tables, does not insert anything
doCreateTables :: IConnection c => [TableCreate] -> c -> IO ()
doCreateTables ts c = do
  -- this needs to be lazy (quickQuery instead of quickQuery') otherwise we get error 2053:
  -- attempting to read a row which there is no result set associated with the statement
  mapM_ (\(TableCreate s) -> quickQuery c s []) ts
  commit c

doAddTable :: IConnection c => TableCreate -> c -> IO ()
doAddTable t = doCreateTables [t]






test = handleSqlError $ do
  c <- connectMySQL defaultMySQLConnectInfo {
         mysqlHost = "localhost"
       , mysqlUser = "badi"
       , mysqlDatabase = "test"
       , mysqlUnixSocket = "/var/run/mysqld/mysqld.sock"
       }

  let ts = [uncurry tableCreate _master_table, newTable (ColDesc "bar float") (TableName "foo")]
      vs = map (\i -> (Tagged i, Tagged i, Tagged i)) [0..10000]
  doAddTable (ts !! 0) c
  printf "Table %d created\n" (0::Int)
  -- doAddTable (ts !! 1) c
  -- printf "Table %d created\n" (1::Int)
  rs <- countRows c (TableName "master")
  printf "Master has %d rows\n" rs
  insertIntoMaster vs c
  rs' <- countRows c (TableName "master")
  printf "Master has %d rows\n" rs'
  disconnect c
