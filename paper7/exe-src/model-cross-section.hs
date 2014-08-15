import Control.Monad

import Data.Metrology.Poly
import Data.Metrology.Synonyms
import Data.Metrology.SI.Prefixes
import Data.Metrology.SI.Units
import Data.Metrology.Z

import Model.Gas

data ExperimentData
  = ExperimentData
  { dataFilename :: FilePath
  , ion      :: ChemicalSpecies
  , target   :: ChemicalSpecies}

massesD :: ExperimentData -> (Double, Double)
massesD xp = (mu, r) 
  where
    m1 = molecularMass (ion xp) # (Gram :/ Mole)
    m2 = molecularMass (target xp) # (Gram :/ Mole)
  
    mu = m1*m2 / (m1+m2)
    r  = m1 / m2
    
    
inputData :: [ExperimentData]
inputData =
   [e "Ar+_Ar.txt" ArPlus Ar ,
    e "H+_H2.txt"  HPlus  H2 ,
    e "H2+_H2.txt" H2Plus H2 ,
    e "H3+_H2.txt" H3Plus H2 ,
    e "N+_N2.txt"  NPlus  N2 ,
    e "N2+_N2.txt" N2Plus N2 ]
   where e = ExperimentData





main :: IO ()
main = do
  mapM_ bucket inputData
  
bucket :: ExperimentData -> IO ()
bucket xp = do
  let fn = dataFilename xp
  con <- readFile $ "material/cross-section/" ++ fn
  let xys :: [(Int, Double)]
      xys = [(bin $ read sx, read sy) |
             [sx,sy] <- map words $ lines con]
      
      bin :: Double -> Int
      bin x = round $ log x / log 10 * 8 
  mapM_ print xys
  print $ massesD xp