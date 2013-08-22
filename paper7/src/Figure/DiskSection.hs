module Figure.DiskSection where

import Data.Reflection.Typed
import Model.Concepts
import Model.Disk.Hayashi 
import Text.Printf
import UnitTyped(val, (*|))
import           UnitTyped.SI.Derived.Length
import UnitTyped.Synonyms

writeSectionData :: FilePath -> IO ()
writeSectionData fn = do
  writeFile fn ret
  where
    ret = concat $ map (++"\n")
      [ concat
        [ printf "%f %f %e\n" x y (z x y)
        | 
          x <- [0.1,0.2..60]
        ] 
      |
        y <- [0,0.1..20]
      ]
          
    z :: Double -> Double -> Double
    z x y = 
      OrbitalRadius `being` x *| astronomicalUnit$
      ZCoordinate `being` y *| astronomicalUnit $      
      val densityGas 