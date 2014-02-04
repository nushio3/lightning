module Main where

import Control.Lens
import Control.Monad
import System.Environment
import Figure.Lime

main :: IO ()
main = do
  argv <- getArgs
  mapM_ (print . (^.imageFileName)) limeConfigSuite
  mapM_ (execLime ("-X" `elem` argv)) limeConfigSuite

