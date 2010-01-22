
module FaH.Tool.Protomol.Output.Predicate (predicate) where

import FaH.Types
import FaH.Exceptions

import Data.Monoid
import Text.Printf

_name = "FaH.Tool.Protomol.Output.Predicate"

addLog' :: Log m => String -> m ()
addLog' = addLog . printf "[%s] %s" _name

predicate :: Monoid a => Tool Bool -> IO a -> Tool a
predicate pred io = do
  ok <- pred
  
  if ok then addLog' "Failed" >> safeLiftIO io
        else addLog' "Succeeded" >> return mempty
