

module FaH.Driver where

import FaH.Tool
import FaH.Types
import FaH.Tools.ProtomolVMDRMSD
import FaH.Util
import FaH.WorkArea


import Data.Tagged

import System.FilePath

import Control.Concurrent
import Control.Monad


doWork :: ProjectParameters -> [Tool] -> IO WorkArea -> IO [Result]
doWork params ts genWA = apply ts . toolInfos params =<< genWA



ti ps = defaultWorkArea >>= \wa -> return $ toolInfos ps wa



go2 i xs dbn =
    let chunks = chunkify i xs
    in do
      chan <- newChan
      forM_ chunks (\c -> forkIO $ work_chunk chan c)
      logger (length chunks)  chan

go2' i z e = go2 i xs undefined -- testdb -- proj10001
    where xs = [ (r,c,parea) | r <- [z..e], c <- [0..0] ]

proj10001 = DBName "PROJ10001_allhs"
testdb = DBName "test2"

testgo2 = go2 1 [(0,0,parea),(1,1,parea)] testdb

chunkify :: Int -> [a] -> [[a]]
chunkify i xs = reverse $ foldl (f i) [[]] xs
    where f i (as:bs) x = if length as >= i then [x] : as : bs
                          else (as ++ [x]) : bs


pps = map (\((rs,cs,l),db) ->
               (db
               , ProjectParameters rs cs (Tagged $ "/afs/crc.nd.edu/user/l/lcls/fah/fahnd01/data01/data" </> l)))
      [ ((999,5,"PROJ10000"), "test2")]
      -- [ ((999,5,"PROJ10001"), "PROJ10001_allhs")]
      -- [ ((4999,0,"PROJ10000"), "PROJ10000_allhs")]


parea = Tagged $ "/afs/crc.nd.edu/user/l/lcls/fah/fahnd01/data01/data/PROJ10001"

-- work_chunk :: Chan (Message String) -> DBName -> [(RunType,CloneType, ProjArea)] -> IO ()
work_chunk chan tis = do
  wa      <- defaultWorkArea

  mapM_ (applyTool (tool chan)) $ map (\ti -> uncurry3 mkToolInfo ti wa) tis

  writeChan chan Finish

  


logger i chan = forever $ do
  msg <- readChan chan
  case msg of
    Finish -> if i <= 1 then putStrLn "Finished" >>  myThreadId >>= killThread >> return ()
              else putStrLn (show (i-1) ++ " left. ") >> logger (i-1)  chan
    Log m  -> do putStrLn m
                 logger i chan



