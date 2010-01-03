

module FaH.Driver where

import FaH.Tool
import FaH.Types

import Data.Tagged


doWork :: ProjectParameters -> [Tool] -> IO WorkArea -> IO [Result]
doWork params ts genWA = apply ts . toolInfos params =<< genWA
