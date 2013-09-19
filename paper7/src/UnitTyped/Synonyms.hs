{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module UnitTyped.Synonyms where

import           UnitTyped
import           UnitTyped.SI
import           UnitTyped.SI.Constants
import           UnitTyped.SI.Meta
import           UnitTyped.SI.Derived
import           UnitTyped.SI.Derived.Length
import           UnitTyped.SI.Derived.Time
import           UnitTyped.SI.Derived.Mass
import qualified UnitTyped.NoPrelude as U

type GramPerCm2 =  Value '[ '(Mass, POne),  '(Length, NTwo)] '[ '(Gram, POne), '(Centi Meter, NTwo) ] 
type GramPerCm3 =  Value '[ '(Mass, POne),  '(Length, NThree)] '[ '(Gram, POne), '(Centi Meter, NThree) ] 

type CmPerSec = Value '[ '(Length, POne),  '(Time, NOne)] '[ '(Centi Meter, POne), '(Second, NOne) ]
type KmPerSec = Value '[ '(Length, POne),  '(Time, NOne)] '[ '(Kilo Meter, POne), '(Second, NOne) ]

type Kg = Value '[ '(Mass, POne)] '[ '(Kilo Gram, POne) ]  

type AU = Value '[ '(Length, POne)] '[ '(AstronomicalUnit, POne) ] 

type Cm = Value '[ '(Length, POne)] '[ '(Centi Meter, POne) ]

type CoulombPerCm2 = 
  Value
    '[ '(Current, POne), '(Length, NTwo), '(Time, POne)]
    '[ '(Ampere, POne), '(Centi Meter, NTwo), '(Second, POne) ] 

type PermeabilityUnit = 
  Value
    '[ '(Mass, POne),  '(Length, POne), '(Time, NTwo), '(Current, NTwo)]
    '[ '(Kilo Gram, POne) , '(Meter, POne), '(Second, NTwo), '(Ampere, NTwo)] 
  
