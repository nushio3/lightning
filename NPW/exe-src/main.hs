module Main where

import Control.Monad
import Data.Array.Repa.IO.BMP
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Set as Set
import Text.Printf
import System.Process

import Field
import Lightning


main :: IO ()
main = gogo
  
  where
    gogo = do
      str0 <- BS.readFile "/dev/urandom"
      let str = take 10 $ concat $ map (show.(flip mod 10).fromEnum) $ BS.unpack str0
      system $ printf "mkdir -p %s" str
      go str 0 sys1
      gogo
      
    sys1 = (bc1,p1)
    bc1 = initialBC
    p1 = diffuse True (initialBC,  initialPotential)
  
    go :: String -> Int -> System -> IO ()
    go dir age sys = do
      sys2 <- proceed sys
      let tdFlag = isTouchDown initialBC (fst $ sys)
          plotFlag = tdFlag || (mod age 32 ==0)
      when plotFlag $ do
        let fn = printf "%s/test%04d.bmp" dir age
        putStrLn fn
        writeImageToBMP fn $ 
          plot (if tdFlag then 160 else 64) sys2
      when (not tdFlag) $ go dir (age+1) sys2
