
module FaH.Tool.Protomol.CountFrames ( CatDCD (..)
                                     , DCDFile (..)
                                     , frames
                                     ) where

import FaH.Types
import FaH.Exceptions

import Data.Tagged
import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec
import System.Process
import System.FilePath

newtype CatDCD = CatDCD FilePath
newtype DCDFile = DCDFile FilePath

framesCount :: Parser Int
framesCount = do
  anyChar `manyTill` (string "Total frames: ")
  read <$> digit `manyTill` newline


frames :: CatDCD -> DCDFile -> Tool Int
frames (CatDCD catdcd) (DCDFile dcd) = do
  wa <- unTagged . workArea <$> getToolInfo
  output <- safeLiftIO $ readProcess catdcd [wa </> dcd] [] 
  case parse framesCount [] output of
    Left e    -> fail $ show e
    Right num -> return num
