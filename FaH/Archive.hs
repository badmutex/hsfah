
module FaH.Archive where

import FaH.Types

import Control.Applicative ((<$>))
import Codec.Compression.BZip
import Text.Printf (printf)

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Check as Tar
import qualified Data.ByteString.Lazy as BS



unpack_tarbz2 :: FilePath -- ^ the tarball
              -> FilePath -- ^ target directory
              -> IO ()
unpack_tarbz2 tarball targetdir = do
  entries <- Tar.read . decompress <$> BS.readFile tarball
  Tar.unpack targetdir entries
