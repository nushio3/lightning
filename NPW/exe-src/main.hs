module Main where

import Control.Monad
import Data.Array.Repa.IO.BMP
import qualified Data.Set as Set

import Field
import Lightning


main :: IO ()
main = go sys1
  
  where
    sys1 = (bc1,p1)
    bc1 = initialBC
    p1 = diffuse True (initialBC,  initialPotential)
  
    go sys = do
      sys2 <- proceed sys
      writeImageToBMP "test.bmp" $ plot sys2
      go sys2
