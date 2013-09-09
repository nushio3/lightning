module Main where

import Control.Monad
import           Data.Reflection.Typed
import Model.Concepts
import Model.Disk.Hayashi 
import Text.Printf
import UnitTyped (mkVal,autoc)
import           UnitTyped.SI.Meta
import           UnitTyped.SI.Derived.Length
import           UnitTyped.Synonyms
import System.Process

main :: IO ()
main = do
  print $ OrbitalRadius `being` outerRadius $ soundSpeed

  system "mkdir -p tmp"

  writeFile "tmp/pv.txt" pvStr

  where
    pvStr :: String
    pvStr = unlines $ do
      ir <- [-300 .. 300]
      guard $ ir /= 0
      let r = fromInteger ir :: Double
      let vstrs :: [String]
          vstrs = do
              iv <- [-100 .. 100]
              let v = fromInteger iv / 10 :: Double
              return $ printf "%e %e %e" r v (relativePVDistribution (mkVal 30e2) (mkVal r) (mkVal (v*1e5)))
      vstrs ++ [""]
                
