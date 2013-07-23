{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Model.Concepts where

import Data.Reflection.Typed
import UnitTyped.Synonyms

data OrbitalRadius = OrbitalRadius
type instance ValTypeOf OrbitalRadius = AU Double

data ZCoordinate = ZCoordinate
type instance ValTypeOf ZCoordinate = AU Double

data DustRadius = DustRadius
type instance ValTypeOf DustRadius = Cm Double
