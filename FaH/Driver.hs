

module FaH.Driver where

import FaH.Tool
import FaH.Types
import FaH.WorkArea


doWork :: ProjectParameters -> [Tool] -> IO [Status]
doWork params ts = do
  wa <- defaultWorkAreak
  apply ts $ mkToolInfos params wa


mkToolInfos :: ProjectParameters -> WorkArea -> [ToolInfo]
mkToolInfos ps wa = map f vs
    where vs = [ (r,c) | r <- [0..runs ps], c <- [0..clones ps] ]
          f (r,c) = createToolInfo r c (location ps) wa
