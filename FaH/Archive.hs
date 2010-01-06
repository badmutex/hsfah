
module FaH.Archive where

import FaH.Types (CatchError (..))

import Control.Applicative ((<$>))
import Codec.Compression.BZip
import Text.Printf (printf)

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Check as Tar
import qualified Data.ByteString.Lazy as BS

-- unpack_tarbz2 :: FilePath -> IO [(FilePath, CatchError BS.ByteString)]
-- unpack_tarbz2 p = do
--   d <- Tar.read . decompress <$> BS.readFile p
--   return $ Tar.foldEntries (\e bs -> ( Tar.entryPath e
--                                      , getContent e ) : bs)
--                            []
--                            (\s -> [("", Left s)])
--                            d

getBSContent :: Tar.EntryContent -> FilePath -> CatchError BS.ByteString
getBSContent (Tar.NormalFile bs _) _ = Right bs
getBSContent _ p = Left $ printf "%s is not a Tar.NormalFile" p

getContent :: Tar.Entry -> CatchError BS.ByteString
getContent e = let p = Tar.entryPath e
               in getBSContent (Tar.entryContent e) p



unpack_tarbz2 :: FilePath -- ^ the tarball
              -> FilePath -- ^ target directory
              -> IO ()
unpack_tarbz2 tarball targetdir = do
  entries <- Tar.read . decompress <$> BS.readFile tarball
  Tar.unpack targetdir entries
