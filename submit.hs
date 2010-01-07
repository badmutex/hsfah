import System.Process
import Control.Monad
import Text.Printf


main = mapM_ sendoff [0,200..5000-200]

sendoff :: Int -> IO ProcessHandle
sendoff i = let cmd = printf "./FaH/Main +RTS -N6 -RTS %d %d %d 2>&1 > %d-%d.log"
                      i (i+200-1) (200`div`5 :: Int) i (i+200-1)
                qsub = printf "echo '%s' | qsub" (cmd :: String)
            in runCommand (qsub :: String) 