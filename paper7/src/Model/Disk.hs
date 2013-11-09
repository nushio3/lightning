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

makeLenses ''Coord

equatorAt :: AU Double -> Coord
equatorAt r = Coord r (mkVal 0) 0

data Disk = Disk {
  inclinationAngle :: Double,
  centralStarMass ::  GramUnit Double,
  gasSurfaceDensity :: Coord -> GramPerCm2 Double,
  temperature :: Coord -> KelvinUnit Double
  }

data DiskPortion = DiskPortion {
  center :: Coord,
  area :: Cm2 Double
  }

densityGas :: Disk -> Coord -> GramPerCm3 Double
densityGas disk pos = autoc $
  factor *| (gasSurfaceDensity disk pos) |/| h
  where
    z = pos ^. altitude
    factor = (2*pi)**(-1/2)
           * (exp(negate $ val (square z |/| (2 *| square h))))
    h = scaleHeight disk pos

soundSpeed :: Disk -> Coord -> CmPerSec Double
soundSpeed disk pos = U.sqrt $ autoc cssq
  where
    cssq :: Cm2PerSec2 Double
    cssq = autoc $ (kB |*| (temperature disk pos)) 
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

