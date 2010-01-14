
module FaH.Project where

import FaH.Types
import FaH.Logging
import FaH.RunMapper

import Control.Concurrent
import Data.Tagged
import System.Directory
import Control.Monad


-- | verifies that the project data is accessible
validate :: FaHProject Unchecked -> IO (Maybe (FaHProject Checked))
validate p = let true = (== True)
                 p' = unTagged p
             in do
               pathsOK <- all true `fmap` mapM doesDirectoryExist [projectPath p', workPath p']
               return $ if pathsOK then Just $ retag p else Nothing


toRunInfo :: FaHProject Checked -> [RunInfo]
toRunInfo p = let p' = unTagged p
                  ppath = projectPath p'
                  wpath = workPath p'
              in [ RunInfo (Tagged r) [ Tagged c | c <- [0..numClones p' - 1] ] (Tagged ppath) (Tagged wpath) |
                   r <- [0..numRuns p' - 1]
                 ]

doProject runinfo tool = do (l,_,chan) <- newLogger
                            mapM (\ri -> runRunMapper runMapper (RunMapper l ri tool)) runinfo

