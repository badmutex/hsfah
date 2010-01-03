

module FaH.Driver where

import FaH.Tool
import FaH.Types
import FaH.Tools.ProtomolVMDRMSD
import FaH.WorkArea

import qualified  Database.HDBC as DB (run,clone)
import Database.HDBC hiding (run, clone)

import Database.HDBC.MySQL

import Data.Tagged

import System.FilePath


doWork :: ProjectParameters -> [Tool] -> IO WorkArea -> IO [Result]
doWork params ts genWA = apply ts . toolInfos params =<< genWA


-- test = let ti = mkToolInfo 411 1 (Tagged "/home/badi/Research/fah/afs-crc-fah/fahnd01/data01/data/PROJ10001") (Tagged "/tmp")
--            tp = Tagged "/home/badi/Research/fah/test/data/PROJ10001/RUN808/CLONE1"
--            tb = unTagged tp </> "results-000.tar.bz2"

--            pps = ProjectParameters {
--                    runs = 1
--                  , clones = 5
--                  , location = Tagged "/home/badi/Research/fah/afs-crc-fah/fahnd01/data01/data/PROJ10001"
--                  }


--        in applyTool tool ti -- doWork pps [tool] defaultWorkArea


ti ps = defaultWorkArea >>= \wa -> return $ toolInfos ps wa

job db params = do
  c <- connectMySQL defaultMySQLConnectInfo {
         mysqlHost = "localhost"
       , mysqlUser = "badi"
       , mysqlDatabase = db
       , mysqlUnixSocket = "/var/run/mysqld/mysqld.sock"
       }

  doWork params [tool c] defaultWorkArea

  disconnect c

go = mapM_ (uncurry job) pps

pps = map (\((rs,cs,l),db) ->
               (db
               , ProjectParameters rs cs (Tagged $ "/home/badi/Research/fah/afs-crc-fah/fahnd01/data01/data" </> l)))
      [ ((1000,6,"PROJ10001"), "PROJ10001_allhs")
      , ((5000,1,"PROJ10000"), "PROJ10000_allhs")]
