
module FaH.Tool.Protomol.Output.Predicate (predicate) where

import FaH.Types
import FaH.Exceptions
import FaH.Tool.Protomol.Generation

import Data.Monoid
import Text.Printf

_name = "FaH.Tool.Protomol.Output.Predicate"

addLog' :: Log m => String -> m ()
addLog' = addLog . printf "[%s] %s" _name

predicate :: Monoid a => Tool Bool -> Tool a -> Tool a
predicate pred io = do
  ok <- pred

  r <- getRunVal
  c <- getCloneVal
  g <- generation

  let l = addLog' . printf "Run %d Clone %d Gen %d %s predicate test" r c g
  
  if ok then l "succeeded" >> io
        else l "Failed" >> return mempty
