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

type FaH' a = StateT TrajInfo (ReaderT (Tool a) IO)

runFaH :: FaH a -> TrajInfo -> ToolInfo -> IO (Either String [a], [String])
runFaH fah trinfo  = runTool (runListT (evalStateT fah trinfo))

-- runFaH' :: FaH' a -> TrajInfo -> Tool a -> ToolInfo -> IO (Either String a, [String])
runFaH'
  :: StateT
       s -- TrajInfo
       (ReaderT
          r (ErrorT String (WriterT [String] (ReaderT ToolInfo IO))))
       a -- 
     -> s
     -> r
     -> ToolInfo
     -> IO (Either String a, [String])
runFaH' fah trinfo tool = runTool (runReaderT (evalStateT fah trinfo) tool)



testtool :: Tool Int
testtool = do
  ti <- ask
  liftIO $ print ti
  return 42


ti = ToolInfo (Tagged 1) (Tagged 2) (Tagged "/tmp/wa") (Tagged "/tmp/ta")
trji = TrajInfo (Tagged 1) (map Tagged [0..5]) (Tagged "/tmp/pa") (Tagged "/tmp/wa")

-- testtrajtool :: TrajTool [Clone]
-- testtrajtool = do
--   (TrajInfo _ cs _ _) <- get
  
--   return cs

type TL a = ListT (ReaderT a Identity)
runTL m r = runIdentity (runReaderT (runListT m) r)


testTL = do
  x <- [1..4]
  y <- [5..10]
  return (x,y)
