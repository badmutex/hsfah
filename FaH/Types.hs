{-# LANGUAGE
  EmptyDataDecls
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , Rank2Types
  #-}

module FaH.Types where

import Control.Concurrent
import Control.Monad.State
import Control.Monad.Error
import Data.Convertible
import Data.Tagged

import Database.HDBC (IConnection, SqlValue)


data PRun
data PClone
data PFrame
data PProjArea
data PWorkArea
data PTrajPath
data PStructId


-- in case these need to be change, alias them here
type RunType = Int
type CloneType = Int

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


instance Convertible b c => Convertible (Tagged a b) c where
    safeConvert = safeConvert . unTagged

-- The info that a tool has access to
data TrajInfo = TrajInfo {
      run         :: Run
    , clone       :: Clone
    , workArea    :: WorkArea
    , projectArea :: ProjArea
    , logger      :: Logger
    }



data ProjectParameters = ProjectParameters {
      runs :: RunType
    , clones :: CloneType
    , location :: ProjArea
    } deriving Show

data Message a = Stop | Msg a
newtype Log = Log String

type ErrorMsg = String
type CatchError a = Either ErrorMsg a

type Worker a b = ErrorT String (StateT a IO) b
type TrajWorker a = Worker TrajInfo a

type Logger = Log -> IO ()




test :: TrajWorker ()
test = do
  ti <- get
  let r = unTagged . run $ ti
  put $ ti { run = Tagged $ r + 1 }

echo :: TrajWorker ()
echo = do
  ti <- get
  liftIO . print $ run ti

run' :: TrajWorker Run
run' = do
  ti <- get
  return $ run ti

test3 = do
  test
  test
  test
  test
  echo
  run'

test2 = let ti = TrajInfo (Tagged 1) (Tagged 2) undefined undefined undefined
        in evalStateT (runErrorT test3) ti