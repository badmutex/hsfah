
module FaH.Util where

import FaH.Types


import Data.Tagged

import System.FilePath ((</>))




trajPath :: ProjArea -> Run -> Clone -> TrajArea
trajPath (Tagged pa) r c = Tagged $ areaPath r c pa

localWorkArea :: WorkArea -> Run -> Clone -> WorkArea
localWorkArea (Tagged wa) r c = Tagged $ areaPath r c wa

areaPath :: Run -> Clone -> FilePath -> FilePath
areaPath (Tagged r)
         (Tagged c)
         p
         = p </> "RUN" ++ show r </> "CLONE" ++ show c

toolInfos :: RunInfo -> [ToolInfo]
toolInfos (RunInfo run clones projarea workarea) =
    map mk clones
        where mk c = ToolInfo { run = run
                              , clone = c
                              , workArea = workarea
                              , trajArea = trajPath projarea run c
                              }

toolInfos' :: FaHProject Checked -> [ToolInfo]
toolInfos' (Tagged p) = [ mk r c | r <- [0..numRuns p - 1], c <- [0..numClones p - 1] ]
    where mk r c = let r' = Tagged r :: Run
                       c' = Tagged c :: Clone
                       mkP = Tagged . areaPath r' c'
                       wa = mkP $ workPath p :: WorkArea
                       ta = mkP $ projectPath p :: TrajArea
                   in ToolInfo { run = r'
                               , clone = c'
                               , workArea = wa
                               , trajArea = ta
                               }
