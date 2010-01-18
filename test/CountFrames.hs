
import FaH.Types
import FaH.Tool.Protomol.CountFrames

import Data.Tagged
import Text.ParserCombinators.Parsec
import System.FilePath



out = "CatDCD 4.0\n\
\dcdplugin) detected standard 32-bit DCD file of native endianness\n\
\dcdplugin) CHARMM format DCD file (also NAMD 2.1 and later)\n\
\dcdplugin) detected standard 32-bit DCD file of native endianness\n\
\dcdplugin) CHARMM format DCD file (also NAMD 2.1 and later)\n\
\Opened file 'ww.dcd' for reading.\n\
\Read 401 frames from file ww.dcd.\n\
\Total frames: 401\n\
\CatDCD exited normally."

-- t = parse framesCount [] out

toolinfo = ToolInfo { run = undefined
                    , clone = undefined
                    , workArea = Tagged "/home/badi/Research/fah/tmp/RUN0/CLONE0"
                    , trajArea = undefined
                    }

toolreader = Tool { toolLogger = undefined
                  , toolInfo = toolinfo
                  }

test = runTool (frames (CatDCD "catdcd") (DCDFile "ww.dcd")) toolreader
