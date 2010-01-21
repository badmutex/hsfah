
module FaH.Tool.Protomol.Output.DB where

import FaH.Types
import FaH.Tool.Protomol.Generation

import Data.List (intercalate)
import Text.Printf


_name = "FaH.Tool.Output.DB"

addLog' :: Log m => String -> m ()
addLog' = addLog . printf "[%s] %s" _name

format :: Char -> Int -> String -> String
format c n fmt = let c' = [c]
           in flip (++) c' . intercalate c' $ replicate n fmt

formatAll' :: Char -> [String] -> Tool String
formatAll' sep vals = do
  rcg <- formatRunCloneGen' sep
  let rest = intercalate [sep] vals
      fmts = intercalate [sep] $ map (\_ -> "%s") vals
      fmt  = "%s" ++ fmts
  return $ printf fmt rcg rest


formatRunCloneGen' :: Char -> Tool String
formatRunCloneGen' sep = do
  r <- getRunVal
  c <- getCloneVal
  g <- generation

  addLog' $ show (r,c,g)

  return $ printf (format sep 3 "%d") r c g

formatRunCloneGen = formatRunCloneGen' '|'
formatAll = formatAll' '|'