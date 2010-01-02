{-# LANGUAGE
  FlexibleInstances
  , MultiParamTypeClasses
  , TypeSynonymInstances
  #-}

module FaH.Tool where

import FaH.Types

import Data.Tagged
import Control.Applicative ((<$>))
import Control.Monad (mapM_, sequence_)
import System.FilePath
import Text.Printf

applyTool :: Tool -> ToolInfo -> Action
applyTool t = t

applyAllTools :: [Tool] -> ToolInfo -> [Action]
applyAllTools ts ps = map (flip applyTool ps) ts

doAllTools :: [Tool] -> ToolInfo -> IO [Status]
doAllTools ts = sequence . applyAllTools ts

doAllTrajs :: [Tool] -> [ToolInfo] -> IO [Status]
doAllTrajs ts trajs = concat <$> mapM (doAllTools ts) trajs

instance Apply Tool ToolInfo Action            where apply = applyTool
instance Apply [Tool] ToolInfo [Action]        where apply = applyAllTools
instance Apply [Tool] ToolInfo (IO [Status])   where apply = doAllTools
instance Apply [Tool] [ToolInfo] (IO [Status]) where apply = doAllTrajs


findTrajPath' :: ProjArea -> Run -> Clone -> TrajPath
findTrajPath' pa r c = let r'  = printf "RUN%d" (unTagged r)
                           c'  = printf "CLONE%d" (unTagged c)
                           pa' = unTagged pa
                       in Tagged $ pa' </> r' </> c'

findTrajPath :: ToolInfo -> TrajPath
findTrajPath ti = findTrajPath' (projectArea ti) (run ti) (clone ti)