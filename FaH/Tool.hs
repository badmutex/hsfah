
module FaH.Tool where

import FaH.Types

import Control.Monad

apply :: Tool -> Project -> Action
apply t = t

applyAll :: [Tool] -> Project -> [Action]
applyAll ts ps = map (flip apply ps) ts

doAll :: [Tool] -> Project -> Action
doAll ts = sequence_ . applyAll ts
