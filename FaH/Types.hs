{-# LANGUAGE
  EmptyDataDecls
  #-}

module FaH.Types ( Run, Clone
                 , ProjArea, WorkArea

                 , Message (..)
                 , Log (..)
                 , Logger

                 , ToolInfo (..)

                 , Tool

                 ) where


import Control.Concurrent (Chan)
import Control.Monad.Error (ErrorT)
import Control.Monad.State (StateT)

import Data.Tagged


data PRun
data PClone
data PProjArea
data PWorkArea

type RunType   = Int
type CloneType = Int

type Run       = Tagged PRun RunType
type Clone     = Tagged PClone CloneType
type ProjArea  = Tagged PProjArea FilePath
type WorkArea  = Tagged PWorkArea FilePath

data Message a = Stop | Msg a

newtype Log    = Log String
type Logger    = Log -> IO ()


data ToolInfo  = ToolInfo {
      run      :: Run
    , clone    :: Clone
    , workArea :: WorkArea
    , logger   :: Logger
    }

data TrajInfo = TrajInfo Run [Clone] WorkArea


type Tool a = ErrorT String (StateT ToolInfo IO) a

type TrajTool a = ErrorT String (StateT (TrajInfo,Tool a) IO) a
