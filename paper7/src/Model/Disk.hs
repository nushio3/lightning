{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Model.Disk where

import           UnitTyped
import           UnitTyped.Synonyms
import qualified UnitTyped.NoPrelude as U
import           Control.Lens
import qualified Control.Lens as Lens
import           UnitTyped
import           UnitTyped.Synonyms


import           Model.Values
import           Model.Gas
import           Text.Authoring
import           Text.Authoring.TH

data Coord = Coord
  { _radius :: AU Double, 
    _altitude :: AU Double, 
    _azimuth :: Double }
 deriving (Eq, Show)
makeClassy ''Coord

equatorAt1au :: Coord
equatorAt1au = Coord (mkVal 1) (mkVal 0) 0

equatorAt :: AU Double -> Coord
equatorAt r = Coord r (mkVal 0) 0

data Disk = Disk {
  _distanceFromEarth :: Pc Double,
  _inclinationAngle  :: Double,
  _centralStarMass   :: GramUnit Double,
  _gasSurfaceDensityField :: Coord -> GramPerCm2 Double,
  _temperatureField       :: Coord -> KelvinUnit Double,
  _lightningAcceleratorField         :: Coord -> VoltPerCm Double
  }
makeClassy ''Disk

gasSurfaceDensity :: Getter Environment (GramPerCm2 Double)
gasSurfaceDensity = Lens.to $ \env ->  env ^. gasSurfaceDensityField $ env^.coord
temperature :: Getter Environment (KelvinUnit Double)
temperature = Lens.to $ \env -> env ^. temperatureField $ env^.coord
lightningAccelerator :: Getter Environment (VoltPerCm Double)
lightningAccelerator = Lens.to $ \env ->  env^.lightningAcceleratorField $ env^.coord

newtype Environment = Environment (Disk,Coord)
(>$<) :: Disk -> Coord -> Environment
a >$< b = Environment (a,b)

makeWrapped ''Environment

                                            
instance HasDisk Environment where disk = unwrapped . _1
instance HasCoord Environment where coord = unwrapped . _2


data DiskPortion = DiskPortion {
  _center :: Coord,
  _area :: Cm2 Double
  } deriving (Eq, Show)
makeClassy ''DiskPortion

splittedDisk :: [DiskPortion]
splittedDisk = 
  [combine clrR clrP |  clrR <- rs, clrP <- phis]
  where
    combine (r0, (rL, rR)) (p0, (pL, pR)) =
      DiskPortion (Coord (mkVal r0) (mkVal 0) p0) a0
        where
          a0 :: Cm2 Double
          a0 = autoc $ dr |*| r |*| dphi
          
          dr :: AU Double
          dr = mkVal (rR - rL)
          dphi :: NoDimension Double
          dphi = mkVal (pR - pL)
          r :: AU Double
          r = mkVal r0
          
          
    rs = toCLR radialBoundaries
    phis = toCLR azimuthalBoundaries
    
    radialBoundaries :: [Double]
    radialBoundaries = [ 10 ** (fromInteger i / 10) | i <- [-10..30]]
    
    azimuthalBoundaries :: [Double]
    azimuthalBoundaries = [(fromInteger i / 20 * pi) | i <- [0..40]]    

    toCLR :: [Double] -> [(Double, (Double, Double))]
    toCLR xs = zipWith f xs (tail xs)
      where
        f l r = ((l+r)/2, (l,r))
        
densityGas :: Getter Environment (GramPerCm3 Double)
densityGas = Lens.to go where 
  go env = autoc $ factor *| (env^.gasSurfaceDensity) |/| h where
      z = pos ^. altitude
      factor = (2*pi)**(-1/2)
             * (exp(negate $ val (square z |/| (2 *| square h))))
      h = env^.scaleHeight
      pos = env^.coord 

numberDensityGas :: Getter Environment (PerCm3 Double)
numberDensityGas = Lens.to go where 
  go env = autoc $ factor *| (env^.gasSurfaceDensity) |/| h |/| molecularMass H2 where
      z = pos ^. altitude
      factor = (2*pi)**(-1/2)
             * (exp(negate $ val (square z |/| (2 *| square h))))
      h = env^.scaleHeight
      pos = env^.coord 


soundSpeed :: Getter Environment (CmPerSec Double)
soundSpeed = Lens.to (go . (^.)) where
  go env = U.sqrt $ autoc cssq where
      cssq :: Cm2PerSec2 Double
      cssq = autoc $ (kB |*| env temperature) 
                 |/| (2.34 *| protonMass)

orbitalAngularVelocity :: Getter Environment (HertzUnit Double)
orbitalAngularVelocity = Lens.to go where
  go env0 = U.sqrt $ autoc $ gravitationalConstant |*| mSun |/| cubic r where
    mSun = env centralStarMass 
    r = env radius
    env = (env0^.)

orbitalVelocity :: Getter Environment (CmPerSec Double)
orbitalVelocity = Lens.to go where
  go  env = U.sqrt $ autoc $ gravitationalConstant |*| mSun |/| r where
    mSun = env ^. centralStarMass 
    r = env ^. radius


scaleHeight :: Getter Environment (AU Double)
scaleHeight = Lens.to go where
  go env =
      autoc $ (env ^. soundSpeed)
          |/| (env ^. orbitalAngularVelocity)

sigmoid :: Double -> Double
sigmoid x = 1/(1+exp (negate x))

gaussian :: Double -> Double -> Double -> Double
gaussian mu sigma x = 1/sqrt(2*pi* sigma^2) * exp (negate $ (/2) $ ((x-mu)/sigma)^2)


ppdDensity :: Getter Environment (GramPerCm3 Double)
ppdDensity = densityGas


ppdNumberDensity :: Getter Environment (PerCm3 Double)
ppdNumberDensity = Lens.to $ 
  \env -> autoc $ (env ^. ppdDensity) |/| ppdMix molecularMass

mfpPpd15 :: Getter Environment (Cm Double)
mfpPpd15 = Lens.to $ \env -> 
  autoc $ 1 /| (env^.ppdNumberDensity) |/| (ppdMix $ inelCrossSection 15)

mfpPpd15E :: Getter Environment (Cm Double)
mfpPpd15E = Lens.to $ \env->
  autoc $ 1 /| (env^.ppdNumberDensity) |/| (ppdMix $ elCrossSection 15)

ppdDielectricStrengthT :: Getter Environment (VoltPerCm Double)
ppdDielectricStrengthT = Lens.to go where
  go env = autoc $ w |/| ((env^.mfpPpd15) |*| elementaryCharge) where
    w = mkVal 15 :: ElectronVolt Double

ppdDielectricStrengthDP :: Getter Environment (VoltPerCm Double)
ppdDielectricStrengthDP = Lens.to go where
  go env = autoc $ ratio *| w |/| (0.43 *| elementaryCharge |*| (env^.mfpPpd15E)) where
    w = mkVal 15                           :: ElectronVolt Double
    ratio = sqrt $ val ratioD              :: Double
    ratioD = autoc $ electronMass |/| bigM :: NoDimension Double
    bigM = autoc $ ppdMix molecularMass    :: GramUnit Double

ppdDielectricStrengthR :: Getter Environment (VoltPerCm Double)
ppdDielectricStrengthR = Lens.to go where
  go env = autoc $ 
    (20.2/(8*pi)) *| (e3 |*| z |*| n)
             |/| (vacuumPermittivity |*| vacuumPermittivity |*| nrg) 
    where
      nrg :: JouleUnit Double
      nrg = autoc $ electronMass |*| speedOfLight |*| speedOfLight
      
      e3 = elementaryCharge |*| elementaryCharge |*| elementaryCharge 
      z = ppdMix atomicNumber
      n = env ^. ppdNumberDensity 

