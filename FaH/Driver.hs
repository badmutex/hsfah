

module FaH.Driver where

import FaH.Tool
import FaH.Types
import FaH.Tools.ProtomolVMDRMSD
import FaH.Util
import FaH.WorkArea

import qualified  Database.HDBC as DB (run,clone)
import Database.HDBC hiding (run, clone)

import Database.HDBC.MySQL

import Data.Tagged

import System.FilePath

import Control.Concurrent
import Control.Monad


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

-- job db params = do
--   c <- connectMySQL defaultMySQLConnectInfo {
--          mysqlHost = "phaeton.cse.nd.edu"
--        , mysqlUser = "cabdulwa"
--        , mysqlDatabase = db
--        -- , mysqlUnixSocket = "/var/run/mysqld/mysqld.sock"
--        }

--   doWork params [tool c] defaultWorkArea

--   disconnect c

-- go = mapM_ (uncurry job) pps


go2 i xs dbn =
    let chunks = chunkify i xs
    in do
      chan <- newChan
      forkIO $ logger chan
      forM_ chunks (\c -> forkIO $ work_chunk chan dbn c)
      forever $ return ()

go2' i = go2 i xs (DBName "PROJ10001_allhs")

xs = [ (r,c,parea) | r <- [64..1000], c <- [0..5] ]

testgo2 = go2 1 [(0,0,parea),(1,1,parea), (2,2,parea)] (DBName "test2")

chunkify :: Int -> [a] -> [[a]]
chunkify i xs = reverse $ foldl (f i) [[]] xs
    where f i (as:bs) x = if length as >= i then [x] : as : bs
                          else (as ++ [x]) : bs
  


pps = map (\((rs,cs,l),db) ->
               (db
               , ProjectParameters rs cs (Tagged $ "/afs/crc.nd.edu/user/l/lcls/fah/fahnd01/data01/data" </> l)))
      [ ((0,0,"PROJ10001"), "test2")]
      -- [ ((999,5,"PROJ10001"), "PROJ10001_allhs")]
      -- [ ((4999,0,"PROJ10000"), "PROJ10000_allhs")]


parea = Tagged $ "/afs/crc.nd.edu/user/l/lcls/fah/fahnd01/data01/data/PROJ10001"

-- work_chunk :: Chan (Message String) -> DBName -> [(RunType,CloneType, ProjArea)] -> IO ()
work_chunk chan  (DBName db) tis = do
  c       <- connectMySQL defaultMySQLConnectInfo {
                             mysqlHost = "phaeton.cse.nd.edu"
                           , mysqlUser = "cabdulwa"
                           , mysqlDatabase = db
                           }

  wa      <- defaultWorkArea
  mapM (applyTool (tool chan c)) $ map (\ti -> uncurry3 mkToolInfo ti wa) tis
  -- doAllTrajs [tool c] $ map (\ti -> uncurry3 mkToolInfo ti wa) tis

  disconnect c

  


logger chan = forever $ do
  msg <- readChan chan
  case msg of
    Finish -> do myThreadId >>= killThread
    Log m  -> do putStrLn m



