
-- | Utility functions

module FaH.Util where

import FaH.Types

import Text.Printf
import Data.Tagged
import System.FilePath

import Control.Concurrent (Chan, ThreadId, myThreadId, killThread, forkIO, readChan, writeChan)
import Control.Monad (forever)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c


loggerThread :: Chan (Message Log) -> IO ThreadId
loggerThread chan = forkIO . forever $ do
  msg <- readChan chan
  case msg of
    Stop        -> myThreadId >>= killThread
    Msg (Log s) -> putStrLn s


logStr :: Chan (Message Log) -> Log -> IO ()
logStr chan l = writeChan chan $ Msg l



findTrajPath' :: ProjArea -> Run -> Clone -> TrajPath
findTrajPath' pa r c = let r'  = printf "RUN%d" (unTagged r)
                           c'  = printf "CLONE%d" (unTagged c)
                           pa' = unTagged pa
                       in Tagged $ pa' </> r' </> c'

trajPath :: TrajInfo -> TrajPath
trajPath ti = findTrajPath' (projectArea ti) (run ti) (clone ti)

toolInfos :: ProjectParameters -> WorkArea -> Logger -> [TrajInfo]
toolInfos ps wa l = map f vs
    where vs = [ (r,c) | r <- [0..runs ps], c <- [0..clones ps] ]
          f (r,c) = mkTrajInfo r c (location ps) wa l


mkTrajInfo :: RunType -> CloneType -> ProjArea -> WorkArea -> Logger -> TrajInfo
mkTrajInfo r c projloc wa loggin = TrajInfo { run         = Tagged r
                                            , clone       = Tagged c
                                            , workArea    = wa
                                            , projectArea = projloc
                                            , logger      = loggin
                                            }
