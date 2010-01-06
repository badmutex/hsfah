module FaH.Worker.Protomol where

import FaH.Archive
import FaH.Types
import FaH.Util

import System.FilePath.Glob
import Data.Tagged
import Data.List (sort)

import Control.Monad.State
import Control.Monad.Error



_tarball_glob = "results-???.tar.bz2"

data ProtomolProjInfo = PPInfo {
      trajInfo :: TrajInfo
    , tarballs :: [FilePath]
    }



protomol :: Worker ProtomolProjInfo b -> TrajWorker b
protomol worker =
    let mkCmd ti tarballs = PPInfo { trajInfo = ti
                                   , tarballs = tarballs }

        get_tarballs :: TrajPath -> IO [FilePath]
        get_tarballs = fmap sort . globDir1 (compile _tarball_glob) . unTagged

    in do
      trajinfo  <- get
      tarballs  <- liftIO . get_tarballs $ trajPath trajinfo
      wesrem worker $ mkCmd trajinfo tarballs



testWorker :: Worker ProtomolProjInfo ()
testWorker = do
  ppi <- get
  liftIO . mapM_ putStrLn $ tarballs ppi
  throwError "42"

testProtomolTraj = let ti = TrajInfo (Tagged 1) (Tagged 2) wa pa undefined
                       pa = Tagged "/home/badi/Research/fah/afs-crc-fah/fahnd01/data01/data/PROJ10001" :: ProjArea
                       wa = Tagged "/tmp/hsfahwa" :: WorkArea
                   in esrem (protomol testWorker) ti
