{-# LANGUAGE
  EmptyDataDecls
  #-}

module FaH.Types ( Run, Clone
                 , ProjArea, TrajArea, WorkArea

                 , Message (..)
                 , Log (..)
                 , Logger

                 , ToolInfo (..), TrajInfo (..)
                 , runTool, runTrajTool, runFaH

                 , Tool
                 , TrajTool
                 , FaH

                 ) where


import Control.Concurrent (Chan)

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.List
import Control.Monad.Identity

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
    } deriving Show


data TrajInfo = TrajInfo Run [Clone] ProjArea WorkArea deriving Show

type Tool = ErrorT String (WriterT [String] (ReaderT ToolInfo IO))
type TrajTool = ErrorT String (WriterT [String] (StateT TrajInfo (ListT IO)))

runTool :: Tool a -> ToolInfo -> IO (Either String a, [String])
runTool t = runReaderT (runWriterT (runErrorT t))

--runTrajTool :: TrajTool a -> TrajInfo -> IO [(Either String a, [String])]
runTrajTool trtool trinfo = runListT (evalStateT (runWriterT (runErrorT trtool)) trinfo)

type FaH = StateT TrajInfo (ListT Tool)


runFaH :: FaH a -> TrajInfo -> ToolInfo -> IO (Either String [a], [String])
runFaH fah trinfo  = runTool (runListT (evalStateT fah trinfo))



