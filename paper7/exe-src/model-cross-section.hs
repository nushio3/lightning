{-# LANGUAGE RecordWildCards, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Control.Monad hiding (mapM, mapM_, forM_,forM)
import Numeric.Optimization.Algorithms.CMAES

import Data.Foldable
import Data.Traversable 
import qualified Data.Map as M
import Data.Maybe
import Data.Metrology.Poly
import Data.Metrology.Synonyms
import Data.Metrology.SI.Prefixes
import Data.Metrology.SI.Units
import Data.Metrology.Z

import Prelude hiding (sum,mapM, mapM_)
import Text.Printf

import Model.Gas

justLookup k m = fromJust $ M.lookup k m

data ExperimentData
  = ExperimentData
  { dataFilename :: FilePath
  , ion      :: ChemicalSpecies
  , target   :: ChemicalSpecies
  , i2cs     :: M.Map Int Double }

data ModelTC a = Model
  { paramA :: a
  , paramP :: a
  , paramQ :: a
  } deriving (Eq, Show, Read, Functor, Foldable, Traversable)

type Model = ModelTC Double

defaultModel = Model 100 0 0

predict :: Model -> ExperimentData -> Double
predict Model{..} xp = paramA * mu**paramP * r**paramQ
  where
    (mu,r) = massesD xp
  
errorAt :: Model -> ExperimentData -> Int -> Double
errorAt m xp i = (log valM - log valX) ** 2
  where
    valM = max 1e-99 $ predict m xp
    valX = justLookup i (i2cs xp)

fitModel :: [ExperimentData] -> Int -> IO Model
fitModel xps i = do
  run $ minimizeT f defaultModel
  where
    f :: Model -> Double
    f m = sum [errorAt m xp i | xp <- xps]

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
   where e f a b = ExperimentData f a b M.empty

toModelFn :: FilePath -> FilePath
toModelFn fn = (++"_model.txt") $ reverse $ drop 4 $ reverse fn



main :: IO ()
main = do
  loadedData <- mapM loadFile inputData
  let idxs = [-8..32] :: [Int]
  ms <- forM idxs $ \i -> do
    m <- fitModel loadedData i
    print m
    return (i,m)
  let models :: M.Map Int Model
      models = M.fromList ms
  forM_ loadedData $ \xp -> do
    let fn2 = toModelFn (dataFilename xp)
        mkLine :: Int -> String
        mkLine i = printf "%f %f %f" 
                   (10 ** (fromIntegral i / 8) :: Double)
                   (justLookup i (i2cs xp))
                   (predict (justLookup i models) xp)
                   
        
    writeFile fn2 $ unlines $
      map mkLine idxs
  return ()
  
loadFile :: ExperimentData -> IO ExperimentData
loadFile xp = do
  let fn = dataFilename xp
  con <- readFile $ "material/cross-section/" ++ fn
  let xys :: [(Int, Double)]
      xys = [(bin $ read sx, read sy) |
             [sx,sy] <- map words $ lines con]
      
      bin :: Double -> Int
      bin x = round $ log x / log 10 * 8 
  mapM_ print xys
  print $ massesD xp
  return xp{i2cs = M.fromList xys}