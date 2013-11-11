module Model.Disk where

import           UnitTyped
import           UnitTyped.Synonyms
import qualified UnitTyped.NoPrelude as U

import           Model.Concepts
import           Model.Values

import           Text.Authoring
import           Text.Authoring.TH


data Disk = Disk {
  _gasSurfaceDensity :: AU Double -> GramPerCm2 Double
  }