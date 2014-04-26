{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Model.Disk where

import           Data.Metrology
import           Data.Metrology.Synonyms
import           Control.Lens
import qualified Control.Lens as Lens


import           Model.Values
import           Model.Gas
import           Text.Authoring
import           Text.Authoring.TH

data Coord = Coord
  { _radius :: Length,
    _altitude :: Length,
    _azimuth :: Double }
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

                                            
instance HasDisk Environment where disk = _Unwrapped . _1
instance HasCoord Environment where coord = _Unwrapped . _2


data DiskPortion = DiskPortion {
  _center :: Coord,
  _area :: Area
  } deriving (Eq, Show)
makeClassy ''DiskPortion

splittedDisk :: [DiskPortion]
splittedDisk = 
  [combine clrR clrP |  clrR <- rs, clrP <- phis]
  where
    combine (r0, (rL, rR)) (p0, (pL, pR)) =
      DiskPortion (Coord (r0 % AU) (0 % AU) p0) a0
        where
          a0 :: Area
          a0 = autoc $ dr |*| r |*| dphi
          
          dr :: Length
          dr = (rR - rL)
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
        
densityGas :: Getter Environment (GramPerCm3 Double)
densityGas = Lens.to go where 
  go env = autoc $ factor *. (env^.gasSurfaceDensity) |/| h where
      z = pos ^. altitude
      factor = (2*pi)**(-1/2)
             * (exp(negate $ val (square z |/| (2 *. square h))))
      h = env^.scaleHeight
      pos = env^.coord 

numberDensityGas :: Getter Environment (PerCm3 Double)
numberDensityGas = Lens.to go where 
  go env = autoc $ factor *. (env^.gasSurfaceDensity) |/| h |/| molecularMass H2 where
      z = pos ^. altitude
      factor = (2*pi)**(-1/2)
             * (exp(negate $ val (square z |/| (2 *. square h))))
      h = env^.scaleHeight
      pos = env^.coord 


soundSpeed :: Getter Environment (CmPerSec Double)
soundSpeed = Lens.to (go . (^.)) where
  go env = U.sqrt $ autoc cssq where
      cssq :: Cm2PerSec2 Double
      cssq = autoc $ (kB |*| env temperature) 
                 |/| (2.34 *. protonMass)

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

ppdDielectricStrengthT :: Getter Environment (QofU VoltPerCm)
ppdDielectricStrengthT = Lens.to go where
  go env = redim $ w |/| ((env^.mfpPpd15) |*| elementaryCharge) where
    w = 15 % ElectronVolt

ppdDielectricStrengthDP :: Getter Environment (QofU VoltPerCm)
ppdDielectricStrengthDP = Lens.to go where
  go env = redim $ ratio *. w |/| (0.43 *. elementaryCharge |*| (env^.mfpPpd15E)) where
    w = 15 % ElectronVolt
    ratio = qSqrt $  ratioD         :: QuOfUL Number MySU
    ratioD =  electronMass |/| bigM :: QuOfUL Number MySU
    bigM = (ppdMix molecularMass |/| avogadroConstant) :: Mass


ppdDielectricStrengthR :: Getter Environment (QofU VoltPerCm)
ppdDielectricStrengthR = Lens.to go where
  go env = redim $ 
    (20.2/(8*pi)) *. (e3 |*| z |*| n)
             |/| (vacuumPermittivity |*| vacuumPermittivity |*| nrg) 
    where
      nrg :: Energy
      nrg = redim $ electronMass |*| speedOfLight |*| speedOfLight
      
      e3 = elementaryCharge |*| elementaryCharge |*| elementaryCharge 
      z = ppdMix atomicNumber
      n = env ^. ppdNumberDensity 

