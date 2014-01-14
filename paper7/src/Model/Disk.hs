{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Model.Disk where

import           UnitTyped
import           UnitTyped.Synonyms
import qualified UnitTyped.NoPrelude as U
import           Control.Lens
import           UnitTyped
import           UnitTyped.Synonyms

import           Model.Values
import           Text.Authoring
import           Text.Authoring.TH

data Coord = Coord
  { _radius :: AU Double, 
    _altitude :: AU Double, 
    _azimuth :: Double }
 deriving (Eq, Show)
makeLenses ''Coord

equatorAt1au :: Coord
equatorAt1au = Coord (mkVal 1) (mkVal 0) 0

equatorAt :: AU Double -> Coord
equatorAt r = Coord r (mkVal 0) 0

data Disk = Disk {
  distanceFromEarth :: Pc Double,
  inclinationAngle  :: Double,
  centralStarMass   :: GramUnit Double,
  gasSurfaceDensityField :: Coord -> GramPerCm2 Double,
  temperatureField       :: Coord -> KelvinUnit Double,
  lightningAcceleratorField         :: Coord -> VoltPerCm Double
  }

gasSurfaceDensity :: Environment ->  GramPerCm2 Double
gasSurfaceDensity (disk, pos) = gasSurfaceDensityField disk pos
temperature :: Environment ->  KelvinUnit Double
temperature (disk, pos) = temperatureField disk pos
lightningAccelerator :: Environment ->  VoltPerCm Double
lightningAccelerator (disk, pos) = lightningAcceleratorField disk pos

type Environment = (Disk, Coord)

data DiskPortion = DiskPortion {
  center :: Coord,
  area :: Cm2 Double
  } deriving (Eq, Show)

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
        
densityGas :: Disk -> Coord -> GramPerCm3 Double
densityGas disk pos = autoc $
  factor *| (gasSurfaceDensityField disk pos) |/| h
  where
    z = pos ^. altitude
    factor = (2*pi)**(-1/2)
           * (exp(negate $ val (square z |/| (2 *| square h))))
    h = scaleHeight disk pos

soundSpeed :: Disk -> Coord -> CmPerSec Double
soundSpeed disk pos = U.sqrt $ autoc cssq
  where
    cssq :: Cm2PerSec2 Double
    cssq = autoc $ (kB |*| (temperatureField disk pos)) 
               |/| (2.34 *| protonMass)

orbitalAngularVelocity :: Disk -> Coord -> HertzUnit Double 
orbitalAngularVelocity disk pos =
  U.sqrt $ autoc $ gravitationalConstant |*| mSun |/| cubic r
  where
    mSun = centralStarMass disk
    r = pos ^. radius
    
orbitalVelocity :: Disk -> Coord -> CmPerSec Double
orbitalVelocity  disk pos =
  U.sqrt $ autoc $ gravitationalConstant |*| mSun |/| r
  where
    mSun = centralStarMass disk
    r = pos ^. radius


scaleHeight :: Disk -> Coord -> AU Double
scaleHeight disk = f
  where 
    f pos = 
      autoc $ (soundSpeed disk pos) 
          |/| (orbitalAngularVelocity disk pos)

sigmoid :: Double -> Double
sigmoid x = 1/(1+exp (negate x))

gaussian :: Double -> Double -> Double -> Double
gaussian mu sigma x = 1/sqrt(2*pi* sigma^2) * exp (negate $ (/2) $ ((x-mu)/sigma)^2)

