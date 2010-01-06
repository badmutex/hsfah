{-# LANGUAGE
  FlexibleContexts
  #-}

module FaH.Worker.DB where

import FaH.Types
import FaH.Util

import Data.Convertible
import Control.Monad.State
import Text.Printf

import qualified  Database.HDBC as DB (run,clone)
import Database.HDBC hiding (run, clone)

import Database.HDBC.MySQL

mkInsertVals :: Convertible a SqlValue => [(Run,Clone,Frame,a)] -> [[SqlValue]]
mkInsertVals = undefined

-- insertWorker :: (IConnection conn, Convertible a SqlValue) =>
--                 TableName -> ColName -> Worker a -> conn -> CatchError ()
insertWorker (TableName tn) (ColName cn) worker conn =
    do res <- worker
       let q = printf "insert into %s (%s,%s) values (?,?)" tn "asdf" cn
           vals = mkInsertVals res
       ps <- prepare conn q
       executeMany ps vals
       commit conn
       return $ Right ()
    `catchSql`
    \e -> return $ Left e