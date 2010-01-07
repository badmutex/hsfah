module Main where

import FaH.Driver

import System.Environment
import System.Process


main = do
  [low,high,jump] <- getArgs
  go2'(read jump) (read low) (read high)
