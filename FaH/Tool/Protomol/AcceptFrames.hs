
module FaH.Tool.Protomol.AcceptFrames where

import FaH.Types
import FaH.Tool.Protomol.CountFrames

accept :: CatDCD -> DCDFile -> FramesType -> Tool Bool
accept catdcd dcdfile acceptable = (==) acceptable <$> frames catdcd dcdfile
