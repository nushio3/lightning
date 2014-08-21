{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Model.Disk where

import           Data.Metrology.Poly
import           Data.Metrology.SI.Poly (Meter(..))
import           Data.Metrology.Show
import           Data.Metrology.Synonyms
import           Control.Lens hiding ((#))
import qualified Control.Lens as Lens


import           Model.Values
import           Model.Gas
import           Text.Authoring
import           Text.Authoring.TH

data Coord = Coord
  { _radius :: Length,
    _altitude :: Length,
    _azimuth :: Double } deriving (Show)
makeClassy ''Coord

equatorAt1au :: Coord
equatorAt1au = Coord (1 % AU) (0 % AU) 0

equatorAt :: Length -> Coord
equatorAt r = Coord r (0 % AU) 0

data Disk = Disk {
  _distanceFromEarth :: Length ,
  _inclinationAngle  :: Double,
  _centralStarMass   :: Mass,
  _gasSurfaceDensityField :: Coord -> QofU GramPerCm2,
  _temperatureField       :: Coord -> Temperature,
  _lightningAcceleratorField         :: Coord -> QofU VoltPerCm 
  }
makeClassy ''Disk

gasSurfaceDensity :: Getter Environment (QofU GramPerCm2)
gasSurfaceDensity = Lens.to $ \env ->  env ^. gasSurfaceDensityField $ env^.coord
temperature :: Getter Environment Temperature
temperature = Lens.to $ \env -> env ^. temperatureField $ env^.coord
lightningAccelerator :: Getter Environment (QofU VoltPerCm)
lightningAccelerator = Lens.to $ \env ->  env^.lightningAcceleratorField $ env^.coord

newtype Environment = Environment (Disk,Coord)
(>$<) :: Disk -> Coord -> Environment
a >$< b = Environment (a,b)

makeWrapped ''Environment

                                            
instance HasDisk Environment where disk = _Wrapped' . _1
instance HasCoord Environment where coord = _Wrapped' . _2


data DiskPortion = DiskPortion {
  _center :: Coord,
  _area :: Area
  } deriving (Show)
makeClassy ''DiskPortion

splittedDisk :: [DiskPortion]
splittedDisk = 
  [combine clrR clrP |  clrR <- rs, clrP <- phis]
  where
    combine (r0, (rL, rR)) (p0, (pL, pR)) =
      DiskPortion (Coord r (0 % AU) p0) a0
        where
          a0 :: Area
          a0 = redim $ dr |*| r |*| dphi
          
          dr :: Length
          dr = (rR - rL) % AU
          dphi :: QofU Number
          dphi = (pR - pL) % Number
          r :: Length
          r = r0 % AU
          
          
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
        
densityGas :: Getter Environment Density
densityGas = Lens.to go where 
  go env = redim $ factor *| (env^.gasSurfaceDensity) |/| h where
      z = pos ^. altitude
      factor = (2*pi)**(-1/2)
             * (exp(negate $ (# Number) (qSq z |/| (2 *| qSq h))))
      h = env^.scaleHeight
      pos = env^.coord 

numberDensityGas :: Getter Environment (QofU PerCm3)
numberDensityGas = Lens.to go where 
  go env = redim $ factor *| (env^.gasSurfaceDensity) |/| h |/| molecularMass H2 |*| avogadroConstant where
      z = pos ^. altitude
      factor = (2*pi)**(-1/2)
             * (exp(negate $ (# Number) (qSq z |/| (2 *| qSq h))))
      h = env^.scaleHeight
      pos = env^.coord 


soundSpeed :: Getter Environment Velocity
soundSpeed = Lens.to go where
  go env = qSqrt cssq where
      cssq :: QofU Cm2PerSec2
      cssq = redim $ (kB |*| (env^.temperature) )
                 |/| (2.34 *| protonMass)

orbitalAngularVelocity :: Getter Environment Frequency
orbitalAngularVelocity = Lens.to go where
  go env = qSqrt $ gravitationalConstant |*| mSun |/| qCube r where
    mSun = env ^. centralStarMass :: Mass
    r = env ^. radius :: Length

orbitalVelocity :: Getter Environment Velocity
orbitalVelocity = Lens.to go where
  go env = redim $ qSqrt $ gravitationalConstant |*| mSun |/| r where
    mSun = env ^. centralStarMass 
    r = env ^. radius


scaleHeight :: Getter Environment Length
scaleHeight = Lens.to go where
  go env =
      redim $ (env ^. soundSpeed)
          |/| (env ^. orbitalAngularVelocity)

sigmoid :: Double -> Double
sigmoid x = 1/(1+exp (negate x))

gaussian :: Double -> Double -> Double -> Double
gaussian mu sigma x = 1/sqrt(2*pi* sigma^2) * exp (negate $ (/2) $ ((x-mu)/sigma)^2)


ppdDensity :: Getter Environment Density
ppdDensity = densityGas


ppdNumberDensity :: Getter Environment (QofU PerCm3)
ppdNumberDensity = Lens.to $ 
  \env -> redim $ (env ^. ppdDensity) |/| ppdMix molecularMass |*| avogadroConstant

mfpPpd15 :: Getter Environment Length
mfpPpd15 = Lens.to $ \env -> 
  redim $ 1 /| (env^.ppdNumberDensity) |/| (ppdMix $ inelCrossSection 15)

mfpPpd15N2 :: QofU ElectronVolt -> Getter Environment Length
mfpPpd15N2 nrg = Lens.to $ \env -> 
  redim $ 1 /| (env^.ppdNumberDensity) |/| ((1e-18 * (eps/0.1)**(-0.273) ) % (Meter :^ pTwo))
  where
    eps :: Double
    eps = nrg # ElectronVolt

mfpPpd15E :: Getter Environment Length
mfpPpd15E = Lens.to $ \env->
  redim $ 1 /| (env^.ppdNumberDensity) |/| (ppdMix $ elCrossSection 15)

ppdDielectricStrengthT :: Getter Environment (QofU VoltPerCm)
ppdDielectricStrengthT = Lens.to go where
  go env = redim $ w |/| ((env^.mfpPpd15) |*| elementaryCharge) where
    w = 15 % ElectronVolt

ppdDielectricStrengthDP :: Getter Environment (QofU VoltPerCm)
ppdDielectricStrengthDP = Lens.to go where
  go env = redim $ ratio |*| w |/| (0.43 *| elementaryCharge |*| (env^.mfpPpd15E)) where
    w = 15 % ElectronVolt
    ratio = qSqrt $  ratioD        :: QofU Number
    ratioD = electronMass |/| bigM :: QofU Number
    bigM = (ppdMix molecularMass |/| avogadroConstant) :: Mass


ppdDielectricStrengthR :: Getter Environment (QofU VoltPerCm)
ppdDielectricStrengthR = Lens.to go where
  go env = redim $ 
    (20.2/(8*pi)) *| (e3 |*| z |*| n)
             |/| (vacuumPermittivity |*| vacuumPermittivity |*| nrg) 
    where
      nrg :: Energy
      nrg = redim $ electronMass |*| speedOfLight |*| speedOfLight
      
      e3 = elementaryCharge |*| elementaryCharge |*| elementaryCharge 
      z = ppdMix atomicNumber
      n = env ^. ppdNumberDensity 

