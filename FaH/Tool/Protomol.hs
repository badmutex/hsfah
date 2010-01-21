{-# LANGUAGE
  NoMonomorphismRestriction
  #-}

module FaH.Tool.Protomol ( protomol ) where

import FaH.Types
import FaH.Archive
import FaH.Util
import FaH.Exceptions

import Control.Applicative ((<$>))
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.List
import Control.Monad.Reader

import Data.List (sort)

import System.FilePath
import System.Path.Glob
import System.Directory

import Text.Printf

_name = "FaH.Tool.Protomol"
_results_glob = "results-???.tar.bz2"
addLog' = addLog . printf "[%s] %s" _name

llocal = lift local
handle_tarball :: Tool a -> FilePath -> Tarball -> Tool a
handle_tarball tool target tball = do
  tinfo <- getToolInfo

  addLog' $ ((printf "handling %s" tball) :: String)

  safeLiftIO $ createDirectoryIfMissing True target
  safeLiftIO $ sys_extract_tarbz2 tball target

  useToolInfo (\ti -> ti { workArea = workArea ti <//> Tagged target }) tool


joinWorkArea :: WorkArea -> WorkArea -> WorkArea
joinWorkArea wa1 wa2 = Tagged $ unTagged wa1 </> unTagged wa2
(<//>) = joinWorkArea

tarballs :: TrajArea -> IO [Tarball]
tarballs tra = sort <$> glob (unTagged tra </> _results_glob)


expand_dir wa = combine (unTagged wa) . takeFileName . dropExtension . dropExtension

protomol :: Tool a -> Tool [a]
protomol tool = do 
  tinfo <- getToolInfo
  r <- getRunVal
  c <- getCloneVal

  addLog' $ (printf "starting run %d clone %d" r c :: String)

  tarballs <- safeLiftIO $ tarballs (trajArea tinfo)

  mapM_ (addLog' . (\tb -> printf "Run %d Clone %d: Found %s " r c (takeFileName tb) :: String) . show) tarballs

  let target = expand_dir (workArea tinfo)
  

  ret <- mapM (\tb -> handle_tarball tool (target tb) tb)  tarballs

  safeLiftIO $ mapM_ (removeDirectoryRecursive . target) tarballs


  return ret
