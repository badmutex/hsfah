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

data CmdParams = CmdParams {
      tarballs :: [FilePath]
    , vmd, psf, dcd, ref, script, outfile :: FilePath
    }


get_tarballs :: TrajPath -> IO [FilePath]
get_tarballs = fmap sort . globDir1 (compile _tarball_glob) . unTagged

mkCmd :: TrajInfo -> [FilePath] -> CmdParams
mkCmd = undefined

protomol :: Worker CmdParams b -> TrajWorker b
protomol worker = do
  ti       <- get
  tarballs <- liftIO . get_tarballs $ trajPath ti
  
  res      <- liftIO $ evalStateT (runErrorT worker) (mkCmd ti tarballs)

  case res of
    Right v -> return v
    Left e  -> throwError e
