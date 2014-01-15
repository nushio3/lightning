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

  forM_ [False,True] $ \exMode -> do
    forM_ [HCOPlus, DCOPlus, N2HPlus] $ \chem -> do
      let 
        fnOut :: String
        fnOut = printf "%s/lineProfile-%s-%s.eps" figureDir (show chem)
                       (if exMode then "ex" else "00")      

      fns <- forM (Nothing: map Just breakdownModels) $ \mbdm -> do
        writeLineProfile exMode chem mbdm
        
      let plotFnsStr :: String
          plotFnsStr = intercalate "," $
            [printf "'%s't '%s' w l lw 2" fn tag | (fn,tag) <- fns]
        
      gnuplot 
        [ "set term postscript enhanced 30 color solid"
        , "set grid"
        , "set xlabel 'velocity [km/s]'"
        , "set ylabel 'spectral flux density [Jy]'"
        , printf "set out '%s'" fnOut
        , "plot " ++ plotFnsStr
        ]

writeLineProfile :: Bool -> ChemicalSpecies -> Maybe BreakdownModel -> IO (FilePath,String)
writeLineProfile exMode chem mbd = do
  hPutStrLn stderr fn
  writeFile fn $ unlines $ map (toLine . (/60)) [-180..180]
  return (fn,tag)
  where 
    fn = printf "%s/chem%s-%s-%s.txt" figureDir (show chem) (maybe "no" show mbd)
                (if exMode then "ex" else "00")
    tag = maybe "no" toTag mbd
    
    toTag TownsendBreakdown = "T"
    toTag DPBreakdown = "DP"
    toTag RunawayBreakdown = "R"

    toLine :: Double -> String
    toLine x = 
      let y = val $ lineProfile disk 2 chem (mkVal $ x) 
      in printf "%f %f" x y

    disk = mmsnModel 
           & distanceFromEarth .~ mkVal 56
           & inclinationAngle .~ (7*pi/180)
           & maybe id (if exMode then lightenedDiskEx else lightenedDisk) mbd
           
                  
