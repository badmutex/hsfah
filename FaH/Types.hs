{-# LANGUAGE
  EmptyDataDecls
  #-}

module FaH.Types ( Run, Clone
                 , ProjArea, TrajArea, WorkArea

                 , Message (..)
                 , Log (..)
                 , Logger

                 , ToolInfo (..), TrajInfo (..)

                 , Tool
                 , TrajTool

                 ) where


import Control.Concurrent (Chan)
import Control.Monad.Error (ErrorT)
import Control.Monad.State (StateT)

import Data.Tagged


data PRun
data PClone
data PProjArea
data PWorkArea
data PTrajArea

type RunType   = Int
type CloneType = Int

type Run       = Tagged PRun RunType
type Clone     = Tagged PClone CloneType
type ProjArea  = Tagged PProjArea FilePath
type WorkArea  = Tagged PWorkArea FilePath
type TrajArea  = Tagged PTrajArea FilePath

data Message a = Stop | Msg a

newtype Log    = Log String
type Logger    = Log -> IO ()


data ToolInfo  = ToolInfo {
      run      :: Run
    , clone    :: Clone
    , workArea :: WorkArea
    , trajArea :: TrajArea
    }


data TrajInfo = TrajInfo Run [Clone] ProjArea WorkArea

type Tool a = ErrorT String (StateT ToolInfo IO) a

type TrajTool a = TrajInfo -> Tool a -> ErrorT String IO a
