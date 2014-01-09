module Main where

import Control.Applicative
import Control.Monad
import Model.Disk
import Model.Disk.Hayashi 
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

data BreakdownModel 
  = Conventional
  | Druyvestyen
  | Runaway

gnuplot :: [String] -> IO ()
gnuplot cmds = do
  (hin,_,_,_)<-runInteractiveCommand "gnuplot"
  hPutStr hin $ unlines cmds
  hClose hin

main :: IO ()
main = do
  system "mkdir -p tmp"
  plotLineProfile
  
plotLineProfile :: IO ()  
plotLineProfile = do
  forM_ [-60..60] $ \idv -> do
    let 
        x = idv / 30
        y = val $ lineProfile disk 2 HCOPlus (mkVal $ x)
        
    printf "%f %f\n" x y
  return ()  
  where 
    disk = mmsnModel{inclinationAngle=0.122}

                  

                  
-- plotBM :: Bool -> BreakdownModel -> IO ()  
-- plotBM isRelative bm = do
--   writeFile fnData pvStr
--   gnuplot 
--     [ "set term postscript enhanced 30 color"
--     , "set pm3d" 
--     , "set pm3d map"
--     , "set xlabel 'position [au]'"
--     , "set ylabel 'velocity [km/s]'"
--     , if isRelative then "set cbrange [-1:1]" else "set cbrange [0:1]"
--     , palStr
--     , printf "set out '%s'" fnOut
--     , printf "splot '%s't ''" fnData
--     ]

--   where
--     flagStr 
--       | isRelative = "R"
--       | otherwise  = "C"
--     tag = case bm of
--       Conventional -> "conventional"
--       Druyvestyen -> "DP"
--       Runaway -> "runaway"
--     fnData = printf "tmp/pv-%s-%s.txt" flagStr tag :: String
--     fnOut = printf "tmp/pv-%s-%s.eps" flagStr tag :: String

--     relaVel :: KmPerSec Double
--     relaVel = case bm of
--       Conventional -> mkVal 5
--       Druyvestyen -> mkVal 0.8
--       Runaway -> mkVal 0.03
    
    
--     func2plot
--       | isRelative = relativePVDistribution
--       | otherwise  = chargedPVDistribution

--     pvStr :: String
--     pvStr = unlines $ do
--       ir <- [-300,-297 .. 300]
--       guard $ ir /= 0
--       let r = fromInteger ir :: Double
--       let vstrs :: [String]
--           vstrs = do
--               iv <- [-100,-98 .. 100]
--               let v = fromInteger iv / 10 :: Double
                  
--               return $ printf "%e %e %e" r v (1e5*func2plot (autoc relaVel) (mkVal r) (mkVal (v*1e5)))
--       vstrs ++ [""]
--     palStr
--       | isRelative = "set palette define (-1 '#000040', -0.5 '#0000ff',-0.1 '#8080ff', 0 'white',0.1 '#ff8080', 0.5 '#ff0000', 1 '#400000')"
--       | otherwise = ""
