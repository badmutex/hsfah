
module FaH.Tool.Protomol where


import FaH.Types
import FaH.Archive
import FaH.Util

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.List

import Data.Tagged

import System.FilePath
import System.FilePath.Glob
import System.Directory

import Text.Printf

_name = "FaH.Tool.Protomol"
_results_glob = "results-???.tar.bz2"

addLog' = addLog . printf "[%s] %s" _name


handle_tarball :: MonadIO m =>
                  WorkArea -> Tarball -> Tool a -> m (Either String a, [String])
handle_tarball wa tball tool = do
  let name = takeFileName . dropExtension . dropExtension
      target = unTagged wa </> name tball

  liftIO $ createDirectory target
  liftIO $ extract_tarbz2 tball target

  liftIO $ runTool tool undefined


tarballs :: TrajArea -> IO [Tarball]
tarballs tra = globDir1 (compile _results_glob) (unTagged tra)


protomol :: Tool a -> FaH b
protomol tool = do
  addLog' "starting"
  
  trinfo <- get
  -- tarballs <- liftIO $ tarballs (
  
  liftIO $ handle_tarball undefined undefined tool
  

  return undefined
