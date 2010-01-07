{-# LANGUAGE
  FlexibleInstances
  , MultiParamTypeClasses
  , TypeSynonymInstances
  #-}

module FaH.Tool where

import FaH.Types
import FaH.WorkArea

import Data.Either (partitionEithers)
import Data.Tagged
import Control.Applicative ((<$>))
import Control.Monad (mapM_, sequence_)
import System.FilePath
import Text.Printf

applyTool :: Tool -> ToolInfo -> Action
applyTool t = t

applyAllTools :: [Tool] -> ToolInfo -> [Action]
applyAllTools ts ps = map (flip applyTool ps) ts

doAllTools :: [Tool] -> ToolInfo -> IO [Result]
doAllTools ts = sequence . applyAllTools ts

doAllTrajs :: [Tool] -> [ToolInfo] -> IO [Result]
doAllTrajs ts trajs = concat <$> mapM (doAllTools ts) trajs

instance Apply  Tool  ToolInfo  Action         where apply = applyTool
instance Apply [Tool] ToolInfo [Action]        where apply = applyAllTools
instance Apply [Tool] ToolInfo   (IO [Result]) where apply = doAllTools
instance Apply [Tool] [ToolInfo] (IO [Result]) where apply = doAllTrajs

findTrajPath' :: ProjArea -> Run -> Clone -> TrajPath
findTrajPath' pa r c = let r'  = printf "RUN%d" (unTagged r)
                           c'  = printf "CLONE%d" (unTagged c)
                           pa' = unTagged pa
                       in Tagged $ pa' </> r' </> c'

trajPath :: ToolInfo -> TrajPath
trajPath ti = findTrajPath' (projectArea ti) (run ti) (clone ti)

toolInfos :: ProjectParameters -> WorkArea -> [ToolInfo]
toolInfos ps wa = map f vs
    where vs = [ (r,c) | r <- [0..runs ps], c <- [0..clones ps] ]
          f (r,c) = mkToolInfo r c (location ps) wa


mkToolInfo :: RunType -> CloneType -> ProjArea -> WorkArea -> ToolInfo
mkToolInfo r c projloc wa = ToolInfo { run         = Tagged r
                                     , clone       = Tagged c
                                     , workArea    = wa
                                     , projectArea = projloc
                                     }


