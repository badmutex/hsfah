
-- | Returns the path to the tarball in the project area currently being handled
module FaH.Tool.Protomol.Tarball (tarball) where

import FaH.Types
import FaH.Tool.Protomol.Generation

import System.FilePath
import Text.Printf

_name = "FaH.Tool.Protomol.Tarball"

addLog' :: Log m => String -> m ()
addLog' = addLog . printf "[%s] %s" _name

name :: GenerationType -> String
name gen = printf "results-%03d.tar.bz2" gen

tarball :: Tool FilePath
tarball = do
  gen <- generation
  ta  <- unTagged . trajArea <$> getToolInfo
  let tb = ta </> name gen

  addLog' tb

  return tb
