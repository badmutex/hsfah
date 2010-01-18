
module FaH.Tool.Protomol.Generation (generation) where

import FaH.Types

import Data.Tagged
import Data.Char

import Control.Applicative ((<$>))
import System.FilePath


-- | Converts the workpath such as "/tmp/wa/results-123" to 123
pathToGenNum :: WorkArea -> Int
pathToGenNum (Tagged p) = read . filter isDigit $ p


-- | Converts the workpath such as "/tmp/wa/results-123" to 123
generation :: Tool Int
generation = pathToGenNum . workArea <$> getToolInfo