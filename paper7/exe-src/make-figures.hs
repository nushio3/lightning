module Main where

import Control.Applicative
import Control.Lens
import Control.Monad


import Data.List (intercalate)
import GlobalConfig
import Model.Breakdown
import Model.Disk
import Model.Disk.Hayashi 
import Model.Disk.Derived
import Model.Gas
import Model.RadiativeTransfer
import Text.Printf
import UnitTyped (mkVal,autoc,val)
import           UnitTyped.SI.Meta
import           UnitTyped.SI.Derived.Length
import           UnitTyped.Synonyms
import System.IO
import System.Process
import Text.Printf

gnuplot :: [String] -> IO ()
gnuplot cmds = do
  (hin,_,_,_)<-runInteractiveCommand "gnuplot"
  hPutStr hin $ unlines cmds
  hClose hin

main :: IO ()
main = do
  system $ printf "mkdir -p %s" figureDir
  plotLineProfile
  
plotLineProfile :: IO ()  
plotLineProfile = do

  forM_ [HCOPlus, DCOPlus, N2HPlus] $ \chem -> do
    let 
      fnOut :: String
      fnOut = printf "%s/lineProfile-%s.eps" figureDir (show chem)
    
    fns <- forM (Nothing: map Just breakdownModels) $ \mbdm -> do
      writeLineProfile chem mbdm
      
    let plotFnsStr :: String
        plotFnsStr = intercalate "," $
          [printf "'%s't '' w l lw 2" fn | fn <- fns]
      
    gnuplot 
      [ "set term postscript enhanced 30 color"
      , "set grid"
      , "set xlabel 'velocity [km/s]'"
      , "set ylabel 'spectral flux density [Jy]'"
      , printf "set out '%s'" fnOut
      , "plot " ++ plotFnsStr
      ]

writeLineProfile :: ChemicalSpecies -> Maybe BreakdownModel -> IO FilePath
writeLineProfile chem mbd = do
  hPutStrLn stderr fn
  writeFile fn $ unlines $ map (toLine . (/60)) [-180..180]
  return fn
  where 
    fn = printf "%s/chem%s-%s.txt" figureDir (show chem) (maybe "no" show mbd)

    toLine :: Double -> String
    toLine x = 
      let y = val $ lineProfile disk 2 chem (mkVal $ x) 
      in printf "%f %f" x y

    disk = mmsnModel 
           & inclinationAngle .~ 0.122
           & maybe id lightenedDisk mbd
           
                  
