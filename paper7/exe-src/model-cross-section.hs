import Control.Monad

data Dataset
  = Dataset
  { csFilename :: FilePath }

files :: [FilePath]
files = ["Ar+_Ar.txt",
         "H+_H2.txt",
         "H2+_H2.txt",
         "H3+_H2.txt",
         "N+_N2.txt",
         "N2+_N2.txt"]



main :: IO ()
main = do
  mapM_ bucket files
  
bucket :: FilePath -> IO ()
bucket fn = do
  con <- readFile $ "material/cross-section/" ++ fn
  let xys :: [(Int, Double)]
      xys = [(bin $ read sx, read sy) |
             [sx,sy] <- map words $ lines con]
      
      bin :: Double -> Int
      bin x = round $ log x / log 10 * 8 
  mapM_ print xys
  