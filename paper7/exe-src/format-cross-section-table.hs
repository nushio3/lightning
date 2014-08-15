#!/usr/bin/env runhaskell

import Data.Char
import Text.Printf

main :: IO ()
main = do
  con <- readFile "material/cross-section/rawdata.txt"
  process con
  
  
process :: String -> IO ()
process con = do
  let blks = parseBlock con
  mapM_ processBlock blks
  
  
processBlock :: [String] -> IO ()
processBlock (hl:con) = do
  let fnCS = "material/cross-section/" ++ hl ++ ".txt"
  writeFile fnCS $ unlines $ map cl2dl con
  where
    cl2dl :: String -> String
    cl2dl str = let
      (h:t) = map read' $ words str
      in printf "%f %f" h (sum t :: Double)

    read' :: String -> Double
    read' str
      | last str == '.' = read $ str ++ "0"
      | otherwise       = read str          

parseBlock :: String -> [[String]]
parseBlock con = do
  pb1 lc
  where
    lc :: [String]
    lc = filter (not . all isSpace) $ lines con
    
    isHeadline :: String -> Bool
    isHeadline = 
      not . null .
      filter (not . flip elem "0123456789. \t")
    
    pb1 [] = []
    pb1 (h:t) 
      | isHeadline h = pb2 [h] t
      | otherwise    = error $ "Unexpected line:" ++ h
    
    pb2 con rem = case rem of
      [] -> [con]
      (rh:rt) | isHeadline rh -> con : pb1 rem
      (rh:rt) -> pb2 (con ++ [rh]) rt