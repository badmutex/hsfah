
module FaH.Worker.TarballMapper where


import FaH.Archive
import FaH.Types
import FaH.Worker.Protomol
import FaH.Util

import Control.Monad.State
import Control.Monad.Error
import Data.Tagged

import System.Directory
import System.FilePath


newtype TarballPath = Tarball FilePath

_tarball_dir_name = "results-unpacked"



handleTarball :: Worker TarballPath b -> TarballPath -> FilePath -> Worker ProtomolProjInfo b
handleTarball worker tb@(Tarball targetdir) tarball = do
  liftIO $ unpack_tarbz2 tarball targetdir
  result <- wesrem worker tb
  liftIO $ removeDirectoryRecursive targetdir
  return result



tarballMapper :: Worker TarballPath b -> Worker b c -> Worker ProtomolProjInfo c
tarballMapper tarballer handler = do
  ppi <- get
  let wa = unTagged . workArea . trajInfo $ ppi -- work area
      tbp = Tarball $ wa </> _tarball_dir_name -- target tarball path
  res <- mapM (\tb -> wesrem (handleTarball tarballer ttp tb) ppi)

  return undefined