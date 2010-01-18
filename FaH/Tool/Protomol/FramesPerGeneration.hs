
module FaH.Tool.Protomol.FramesPerGeneration ( module FaH.Tool.Protomol.CountFrames
                                             , module FaH.Tool.Protomol.Generation
                                             , framesPerGeneration
                                             ) where

import FaH.Types
import FaH.Exceptions
import FaH.Tool.Protomol.Generation
import FaH.Tool.Protomol.CountFrames

import Text.Printf

type Generation = Int


_name = "FaH.Tool.Protomol.FramesPerGeneration"

addLog' :: Log m => String -> m ()
addLog' = addLog . printf "[%s] %s" _name


-- | Determines the number of frames in a given generation. 
--   A generation is a 'results-123.tar.bz2' file where '123' is the generation number.
framesPerGeneration :: CatDCD -> DCDFile -> Tool (GenerationType,FramesType)
framesPerGeneration catdcd dcd = do
  gen    <- generation
  frames <- frames catdcd dcd

  r      <- getRunVal
  c      <- getCloneVal

  addLog' $ printf "Run %d Clone %d Gen %d Frames %d" r c gen frames

  return (gen,frames)