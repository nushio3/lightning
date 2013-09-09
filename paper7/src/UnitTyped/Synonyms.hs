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

type GramPerCm2 a =  Value '[ '(Mass, POne),  '(Length, NTwo)] '[ '(Gram, POne), '(Centi Meter, NTwo) ] a
type GramPerCm3 a =  Value '[ '(Mass, POne),  '(Length, NThree)] '[ '(Gram, POne), '(Centi Meter, NThree) ] a

type CmPerSec a = Value '[ '(Length, POne),  '(Time, NOne)] '[ '(Centi Meter, POne), '(Second, NOne) ] a
type KmPerSec a = Value '[ '(Length, POne),  '(Time, NOne)] '[ '(Kilo Meter, POne), '(Second, NOne) ] a

type AU a = a :| AstronomicalUnit

type Cm a = a :| Centi Meter

type CoulombPerCm2 a = 
  Value
    '[ '(Current, POne), '(Length, NTwo), '(Time, POne)]
    '[ '(Ampere, POne), '(Centi Meter, NTwo), '(Second, POne) ] a
