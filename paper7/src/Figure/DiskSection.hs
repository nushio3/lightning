module Figure.DiskSection where

import Text.Printf

writeSectionData :: FilePath -> IO ()
writeSectionData fn = do
  writeFile fn ret
  where
    ret = concat $ map (++"\n")
      [ concat
        [ printf "%f %f %f\n" x y (z x y)
        | 
          x <- [0,0.1..10]
        ] 
      |
        y <- [0,0.1..10]
      ]
          
    z :: Double -> Double -> Double
    z x y = sin(x) * cos(y)
    