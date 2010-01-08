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
import Control.Monad

import Data.List (sort)
import Data.Tagged

import System.FilePath
import System.FilePath.Glob
import System.Directory

import Text.Printf

_name = "FaH.Tool.Protomol"
_results_glob = "results-???.tar.bz2"
addLog' = addLog . printf "[%s] %s" _name


handle_tarball :: Tool a -> WorkArea -> Tarball -> Tool a
handle_tarball tool wa tball = do
  let name = takeFileName . dropExtension . dropExtension -- drop '.tar.bz2'
      target = unTagged wa </> name tball

  addLog' $ ((printf "handling %s" tball) :: String)

  liftIO $ createDirectory target
  liftIO $ extract_tarbz2 tball target


  tool




tarballs :: TrajArea -> IO [Tarball]
tarballs tra = sort <$> globDir1 (compile _results_glob) (unTagged tra)


protomol :: Tool a -> Tool [a]
protomol tool = do
  addLog' "starting"
  
  tinfo <- getToolInfo
  tarballs <- liftIO $ tarballs (trajArea tinfo)

  addLog' $ "tarballs: " ++ show tarballs
  addLog' $ "toolinfo: " ++ show tinfo
  

  ret <- mapM (handle_tarball tool (workArea tinfo)) tarballs

  return ret

  


testp = let ti = ToolInfo (Tagged 808) (Tagged 1) (Tagged "/tmp/wa") (Tagged "/home/badi/Research/fah/test/data/PROJ10001/RUN808/CLONE1")
        in do r <- runTool (protomol testrmsd) ti
              mapM_ putStrLn (snd r)

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