{-# LANGUAGE TemplateHaskell #-}
module Model.Disk where

import           UnitTyped
import           UnitTyped.Synonyms
import qualified UnitTyped.NoPrelude as U
import           Control.Lens


import           Model.Concepts
import           Model.Values

import           Text.Authoring
import           Text.Authoring.TH


data Disk = Disk {
  _inclinationAngle :: Double,
  _gasSurfaceDensity :: AU Double -> GramPerCm2 Double,
  _temperature :: AU Double -> AU Double -> KelvinUnit Double
  }

makeLenses ''Disk
