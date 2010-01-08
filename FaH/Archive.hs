
module FaH.Archive where

import FaH.Types

import Control.Applicative    ((<$>))
import Codec.Compression.BZip (decompress)
import Text.Printf            (printf)

import System.Process
import System.Exit

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Check as Tar
import qualified Data.ByteString.Lazy    as BS


type Tarball   = FilePath
type TargetDir = FilePath


sys_extract_tarbz2 :: Tarball -> TargetDir -> IO  ()
sys_extract_tarbz2 tarball targetdir = do
  let cmd = printf "cd %s; tar jxf %s" targetdir tarball
  e <- waitForProcess =<< runCommand cmd
  case e of
    ExitSuccess -> return ()
    ExitFailure c -> fail $ printf "Command \"%s\" failed with %d" cmd c


extract_tarbz2 :: Tarball -> TargetDir -> IO ()
extract_tarbz2 tarball targetdir = do
  entries <- Tar.read . decompress <$> BS.readFile tarball
  Tar.unpack targetdir (Tar.checkSecurity entries)





read_tarbz2_file :: Tarball -> String -> FilePath -> IO (Either String ())
read_tarbz2_file tb name target = do unpacked <- unpack_tarbz2 tb
                                     case lookup name unpacked of
                                       Nothing -> return $ Left $ printf "%s not found in %s" name tb
                                       Just bs -> case bs of
                                                    Left e -> return $ Left e
                                                    Right bs' -> BS.writeFile target bs' >>= return . Right

unpack_tarbz2 :: FilePath -> IO [(FilePath, Either String BS.ByteString)]
unpack_tarbz2 p = do
  d <- Tar.read . decompress <$> BS.readFile p
  return $ Tar.foldEntries (\e bs -> ( Tar.entryPath e
                                     , getContent e ) : bs)
                           []
                           (\s -> [("", Left s)])
                           d
 
getBSContent :: Tar.EntryContent -> FilePath -> Either String BS.ByteString
getBSContent (Tar.NormalFile bs _) _ = Right bs
getBSContent _ p = Left $ printf "%s is not a Tar.NormalFile" p
 
getContent :: Tar.Entry -> Either String BS.ByteString
getContent e = let p = Tar.entryPath e
               in getBSContent (Tar.entryContent e) p



path :: Int -> String
path = printf "/home/badi/Research/fah/afs-crc-fah/fahnd01/data01/data/PROJ10001/RUN0/CLONE0/results-%03d.tar.bz2"
f = "/tmp/results-000.tar.bz2"

test = extract_tarbz2 f "/tmp"
          



