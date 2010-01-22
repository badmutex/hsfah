
import FaH
import FaH.Tool.ProtomolTools


proj :: FaHProject Unchecked
proj = Tagged $ Project { projectPath = "/home/badi/Research/fah/tmp"
                        , workPath = "/tmp/hsfah"
                        , numRuns = 5
                        , numClones = 6
                        }

catdcd = CatDCD "catdcd"
dcdfile = DCDFile "ww.dcd"
nframes = 401

thetool = protomol $ do
            okq <- accept catdcd dcdfile nframes
            fs  <- frames catdcd dcdfile
            addLog =<< formatAll' ' ' [show fs, show okq]


go = doProject proj thetool (Tagged 0, Tagged 1)
