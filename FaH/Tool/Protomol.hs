{-# LANGUAGE
  NoMonomorphismRestriction
  #-}

module FaH.Tool.Protomol where

import FaH.Tool.Protomol.VMD.RMSD hiding (_name,addLog',testrmsd)


import FaH.Types
import FaH.Archive
import FaH.Util

import Control.Applicative ((<$>))
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.List
import Control.Monad.Reader

import Data.List (sort)
import Data.Tagged

import System.FilePath
import System.FilePath.Glob
import System.Directory

import Text.Printf

_name = "FaH.Tool.Protomol"
_results_glob = "results-???.tar.bz2"
addLog' = addLog . printf "[%s] %s" _name


handle_tarball :: Tool a -> FilePath -> Tarball -> Tool a
handle_tarball tool target tball = do
  tinfo <- getToolInfo

  addLog' $ ((printf "handling %s" tball) :: String)

  liftIO $ createDirectoryIfMissing True target
  liftIO $ sys_extract_tarbz2 tball target

  local (\ti -> ti {workArea = workArea ti <//> Tagged target}) tool



joinWorkArea :: WorkArea -> WorkArea -> WorkArea
joinWorkArea wa1 wa2 = Tagged $ unTagged wa1 </> unTagged wa2
(<//>) = joinWorkArea

tarballs :: TrajArea -> IO [Tarball]
tarballs tra = sort <$> globDir1 (compile _results_glob) (unTagged tra)


expand_dir wa = combine (unTagged wa) . takeFileName . dropExtension . dropExtension

protomol :: Tool a -> Tool [a]
protomol tool = do 
  tinfo <- getToolInfo

  addLog' $ (printf "starting run %d clone %d" (unTagged . run $ tinfo) (unTagged . clone $ tinfo) :: String)

  tarballs <- liftIO $ tarballs (trajArea tinfo)

  addLog' $ "tarballs: " ++ show tarballs
  addLog' $ "toolinfo: " ++ show tinfo


  let target = expand_dir (workArea tinfo)
  

  ret <- mapM (\tb -> handle_tarball tool (target tb) tb)  tarballs

  liftIO $ mapM_ (removeDirectoryRecursive . target) tarballs


  return ret

  


testp = let ti = ToolInfo (Tagged 808) (Tagged 1) (Tagged "/tmp/wa/") (Tagged "/home/badi/Research/fah/afs-crc-fah/fahnd01/data01/data/PROJ10001/RUN0/CLONE0")
        in do removeDirectoryRecursive "/tmp/wa"
              createDirectory "/tmp/wa"
              r <- runTool (protomol testrmsd) ti
              print r
              -- mapM_ putStrLn (snd r)

testrmsd = let ti = ToolInfo r c wa undefined 
               r = Tagged 1
               c = Tagged 2
               wa = Tagged "/tmp/test"
               fileinfo = FileInfo { vmd_bin = "vmd"
                                   , psfpath = "/tmp/test/ww.psf"
                                   , foldedpath = "/tmp/test/ww_folded.pdb"
                                   , scriptname = "rmsd.tcl"
                                   , resultsname = "rmsd.out"
                                   , dcdname = "ww.dcd"
                                   , atomselect = Tagged "all"
                                   , screenoutput = DevNull
                                   }
               genparams = genParams fileinfo
               remove ps = [script ps, outfile ps]
           in (rmsd genparams remove)
