
module FaH.Archive where

import FaH.Types

import Control.Applicative    ((<$>))
import Codec.Compression.BZip (decompress)
import Text.Printf            (printf)

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Check as Tar
import qualified Data.ByteString.Lazy    as BS


type Tarball   = FilePath
type TargetDir = FilePath


unpack_tarbz2 :: Tarball -> TargetDir -> IO ()
unpack_tarbz2 tarball targetdir = do
  entries <- Tar.read . decompress <$> BS.readFile tarball
  Tar.unpack targetdir entries
