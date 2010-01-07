
module FaH.Tool.Protomol where


import FaH.Types
import FaH.Archive
import FaH.Util

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.List

import Data.Tagged

import System.FilePath
import System.Directory



handle_tarball :: WorkArea -> Tarball -> Tool a -> ErrorT String IO a
handle_tarball wa tball tool = do
  let name = takeFileName . dropExtension . dropExtension
      target = unTagged wa </> name tball

  liftIO $ createDirectory target
  liftIO $ extract_tarbz2 tball target

  lift $ runStateT tool undefined

  -- liftIO $ removeDirectoryRecursive target

  return undefined






protomol :: TrajTool a
protomol trajinfo tool = do
  
  
  

  return undefined