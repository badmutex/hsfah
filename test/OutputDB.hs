

import FaH
import FaH.Tool.ProtomolTools

import Text.Printf

catdcd = CatDCD "/home/badi/apps/vmd/bin/catdcd"
dcdfile = DCDFile "ww.dcd"

proj :: FaHProject Unchecked
proj = Tagged $ Project { projectPath = "/home/badi/Research/fah/tmp"
                        , workPath = "/tmp/hsfah"
                        , numRuns = 5
                        , numClones = 6
                        }


thetool :: Tool [()]
thetool = protomol $ do
  (_,f) <- framesPerGeneration catdcd dcdfile
  mtime <- tarballModificationTime
  fmt <- formatAll [show f, show mtime]
  addLog fmt

go = doProject proj thetool (Tagged 0, Tagged 1)