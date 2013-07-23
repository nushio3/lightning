module Figure.Shake where

import Development.Shake
import Development.Shake.FilePath
import GlobalConfig(workDir)

rulesFigures :: Rules ()
rulesFigures = do
  workDir </> "fig1.eps" *> \outFn -> do
    liftIO $ writeFile outFn "hoge\n"
    let motoFn = workDir </> "dat1.txt"
    need [motoFn]
  workDir </> "dat1.txt" *> \outFn -> do
    liftIO $ writeFile outFn "huga\n"    