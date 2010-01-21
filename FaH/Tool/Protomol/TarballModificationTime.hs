

module FaH.Tool.Protomol.TarballModificationTime (tarballModificationTime) where

import FaH.Types
import FaH.Exceptions
import FaH.Tool.Protomol.Tarball

import System.Posix.Types
import System.Posix.Files

tarballModificationTime :: Tool EpochTime
tarballModificationTime = do
  path   <- tarball
  status <- safeLiftIO $ getFileStatus path

  return $ modificationTime status