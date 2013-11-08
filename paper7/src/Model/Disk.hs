{-# LANGUAGE TemplateHaskell #-}
module Model.Disk where

import           UnitTyped
import           UnitTyped.Synonyms
import qualified UnitTyped.NoPrelude as U
import           Control.Lens

import           Model.Values

import           Text.Authoring
import           Text.Authoring.TH

data Coord = Coord
  { _radius :: AU Double, 
    _altitude :: AU Double }

makeLenses ''Coord

equatorAt :: AU Double -> Coord
equatorAt r = Coord r $ mkVal 0

data Disk = Disk {
  inclinationAngle :: Double,
  centralStarMass ::  GramUnit Double,
  gasSurfaceDensity :: Coord -> GramPerCm2 Double,
  temperature :: Coord -> KelvinUnit Double
  }

