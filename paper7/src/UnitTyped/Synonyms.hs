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

type NoDimension = Value '[] '[]

type GramPerCm2 =  Value '[ '(Mass, POne),  '(Length, NTwo)] '[ '(Gram, POne), '(Centi Meter, NTwo) ] 
type GramPerCm3 =  Value '[ '(Mass, POne),  '(Length, NThree)] '[ '(Gram, POne), '(Centi Meter, NThree) ] 

type CmPerSec = Value '[ '(Length, POne),  '(Time, NOne)] '[ '(Centi Meter, POne), '(Second, NOne) ]
type MeterPerSec = Value '[ '(Length, POne),  '(Time, NOne)] '[ '(Meter, POne), '(Second, NOne) ]

type Meter2PerSec2 = Value '[ '(Length, PTwo),  '(Time, NTwo)] '[ '(Meter, PTwo), '(Second, NTwo) ]
type Sec2PerMeter2 = Value '[ '(Length, NTwo),  '(Time, PTwo)] '[ '(Meter, NTwo), '(Second, PTwo) ]


type KmPerSec = Value '[ '(Length, POne),  '(Time, NOne)] '[ '(Kilo Meter, POne), '(Second, NOne) ]

type Kg = Value '[ '(Mass, POne)] '[ '(Kilo Gram, POne) ]  

type GramOf = Value '[ '(Mass, POne)] '[ '(Gram, POne) ]  

type AU = Value '[ '(Length, POne)] '[ '(AstronomicalUnit, POne) ] 

type Cm = Value '[ '(Length, POne)] '[ '(Centi Meter, POne) ]

type CoulombPerCm2 = 
  Value
    '[ '(Current, POne), '(Length, NTwo), '(Time, POne)]
    '[ '(Ampere, POne), '(Centi Meter, NTwo), '(Second, POne) ] 

type Coulomb = 
  Value
    '[ '(Current, POne), '(Time, POne)]
    '[ '(Ampere, POne), '(Second, POne) ] 


type PermittivityUnit = 
  Value
    '[ '(Mass, NOne),  '(Length, NThree), '(Time, PFour), '(Current, PTwo)]
    '[ '(Kilo Gram, NOne) , '(Meter, NThree), '(Second, PFour), '(Ampere, PTwo)] 


type PermeabilityUnit = 
  Value
    '[ '(Mass, POne),  '(Length, POne), '(Time, NTwo), '(Current, NTwo)]
    '[ '(Kilo Gram, POne) , '(Meter, POne), '(Second, NTwo), '(Ampere, NTwo)] 



  
