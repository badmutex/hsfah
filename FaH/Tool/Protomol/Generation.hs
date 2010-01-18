
module FaH.Tool.Protomol.Generation (GenerationType, generation) where

import FaH.Types

import Data.Tagged
import Data.Char

import Control.Applicative ((<$>))
import System.FilePath


type GenerationType = Int


-- | Converts the workpath such as '/tmp/wa/results-123' to 123
pathToGenNum :: WorkArea -> GenerationType
pathToGenNum (Tagged p) = read . filter isDigit . takeBaseName $ p


-- | Converts the workpath such as '/tmp/wa/results-123' to 123
generation :: Tool GenerationType
generation = pathToGenNum . workArea <$> getToolInfo