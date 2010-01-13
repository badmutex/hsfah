

import FaH.Types
import FaH.Exceptions
import FaH.Logging


import Control.Concurrent

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans



bad = readFile "/tmp/bad"
ok  = readFile "/tmp/ok"

testtool :: Tool String
testtool = do addLog "running"
              safeLiftIO bad



testf = do (l,_,chan) <- newLogger
           r <- runTool testtool (Tool l undefined)
           threadDelay 100000
           print r
           finish chan
