{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module UnitTyped.Synonyms where

import           UnitTyped
import           UnitTyped.SI
import           UnitTyped.SI.Constants
import           UnitTyped.SI.Meta
import           UnitTyped.SI.Derived.Length
import           UnitTyped.SI.Derived.Time
import qualified UnitTyped.NoPrelude as U

type GPerCM2 a =  Value '[ '(Mass, POne),  '(Length, NTwo)] '[ '(Gram, POne), '(Centi Meter, NTwo) ] a