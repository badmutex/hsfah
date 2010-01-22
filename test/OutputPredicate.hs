
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

pred' :: Tool Bool
pred' = accept catdcd dcdfile nframes



thetool = protomol $ do
            mtime <- tarballModificationTime
            fmt <- flip (++) "\n" <$> formatAll [show mtime]
            predicate pred' (appendFile "/tmp/pred.out" fmt)

go = doProject proj thetool (Tagged 0, Tagged 1)
