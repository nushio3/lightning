module Figure.Shake where

import Development.Shake
import Development.Shake.FilePath
import Figure.DiskSection (writeSectionData)
import Figure.Gnuplot (gnuplot)
import GlobalConfig 
import Text.Printf

rulesFigures :: Rules ()
rulesFigures = do
  figureDir </> "fig1.eps" *> \outFn -> do
    let motoFn = workDir </> "dat1.txt"
    need [motoFn]
    liftIO $ gnuplot
      [ "set term postscript enhanced color" 
      , printf "set out '%s'" outFn
      , "set pm3d"
      , "set pm3d map"        
      , "set size ratio -1"                
      , "set log cb"        
      , "set format cb '10^{%L}'" -- %3.2l x                
      , "set cbrange [1e-20:1e-10]"        
      , printf "splot '%s'" motoFn 
      ]
    
  figureDir </> "dat1.txt" *> \outFn -> do
    liftIO $ writeSectionData outFn 