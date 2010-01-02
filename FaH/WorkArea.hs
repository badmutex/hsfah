
module FaH.WorkArea where

import FaH.Constants (_workarea_name,_default_workarea_location)

import Control.Applicative ((<$>))
import Control.Monad
import System.FilePath
import System.Random


-- | Generate a path to the workarea
mkWorkArea :: FilePath -> String -> IO FilePath
mkWorkArea root n = do
  g <- getStdGen
  let (g0,g1) = split g
      (g2,g3) = split g1

      uletters = take 4 $ randomRs ('A','Z') g1
      lletters = take 4 $ randomRs ('a','z') g0
      (numbers,_)  = randomR (0,65534) g2 :: (Int, StdGen)

      name = root </> n ++ uletters ++ lletters ++ show numbers

  setStdGen g3

  return name


defaultWorkAreak :: IO String
defaultWorkAreak = mkWorkArea _default_workarea_location _workarea_name
