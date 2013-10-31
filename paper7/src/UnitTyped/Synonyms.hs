{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module UnitTyped.Synonyms where

import           Text.Printf
import           UnitTyped
import           UnitTyped.SI
import           UnitTyped.SI.Constants
import           UnitTyped.SI.Meta
import           UnitTyped.SI.Derived
import           UnitTyped.SI.Derived.Length
import           UnitTyped.SI.Derived.Time
import           UnitTyped.SI.Derived.Mass
import qualified UnitTyped.NoPrelude as U

----------------------------------------------------------------
-- Pretty Printing Functions
----------------------------------------------------------------

ppValF :: PrintfArg x => String -> Value a b x -> String
ppValF fmtStr (Value x) = printf fmtStr x

ppValE :: PrintfArg x => Int -> Value a b x -> String
ppValE d (Value x) = ret
  where
    fmtStr :: String
    fmtStr = printf "%%.%de" d
    
    protoStr :: String
    protoStr = printf fmtStr x

    (valPart,expPart) = break (=='e') protoStr
    
    ret = case expPart of
      "e0" -> valPart
      _ -> printf "%s \\times 10^{%s}" valPart (drop 1 expPart)


----------------------------------------------------------------
-- Nondimensionals
----------------------------------------------------------------

type NoDimension = Value '[] '[]

----------------------------------------------------------------
-- Nonweighted units
----------------------------------------------------------------

type PerSecond =  Value '[ '(Time, NOne)] '[ '(Second, NOne) ] 
type PerCm3 =  Value '[ '(Length, NThree)] '[ '(Centi Meter, NThree) ] 
type PerCm2 =  Value '[ '(Length, NTwo)] '[ '(Centi Meter, NTwo) ] 
type PerCm =  Value '[ '(Length, NOne)] '[ '(Centi Meter, NOne) ] 

----------------------------------------------------------------
-- Weighted units
----------------------------------------------------------------

-- densities
type GramPerCm2 =  Value '[ '(Mass, POne),  '(Length, NTwo)] '[ '(Gram, POne), '(Centi Meter, NTwo) ] 
type GramPerCm3 =  Value '[ '(Mass, POne),  '(Length, NThree)] '[ '(Gram, POne), '(Centi Meter, NThree) ] 




-- energies
type ElectronVolt = Value Energy '[ '(Ev, POne)]
type JouleUnit = Value Energy  '[ '(Joule, POne)]

type JouleSecond = Value '[ '(Time, ('Neg 'One)), '(Mass, ('Pos 'One)), '(Length, ('Pos ('Suc 'One))) ] '[ '(Joule, ('Pos 'One)), '(Second, ('Pos 'One)) ]


type KmPerSec = Value '[ '(Length, POne),  '(Time, NOne)] '[ '(Kilo Meter, POne), '(Second, NOne) ]

type Kg = Value '[ '(Mass, POne)] '[ '(Kilo Gram, POne) ]  

type GramUnit = Value '[ '(Mass, POne)] '[ '(Gram, POne) ]  
type GramPerMole = Value '[ '(Mass, POne)] '[ '(Gram, POne), '(Mole, NOne) ]  

type KelvinUnit = Value '[ '(Temperature, POne)] '[ '(Kelvin, POne) ] 

type AU = Value '[ '(Length, POne)] '[ '(AstronomicalUnit, POne) ] 

type Cm = Value '[ '(Length, POne)] '[ '(Centi Meter, POne) ]

-- velocities
type CmPerSec = Value '[ '(Length, POne),  '(Time, NOne)] '[ '(Centi Meter, POne), '(Second, NOne) ]
type MeterPerSec = Value '[ '(Length, POne),  '(Time, NOne)] '[ '(Meter, POne), '(Second, NOne) ]

-- squared velocities
type Cm2PerSec2 = Value '[ '(Length, PTwo),  '(Time, NTwo)] '[ '(Centi Meter, PTwo), '(Second, NTwo) ]
type Meter2PerSec2 = Value '[ '(Length, PTwo),  '(Time, NTwo)] '[ '(Meter, PTwo), '(Second, NTwo) ]
type Sec2PerMeter2 = Value '[ '(Length, NTwo),  '(Time, PTwo)] '[ '(Meter, NTwo), '(Second, PTwo) ]

-- areas
type Meter2 = Value '[ '(Length, PTwo)] '[ '(Meter, PTwo)]
type Cm2 = Value '[ '(Length, PTwo)] '[ '(Centi Meter, PTwo)]



----------------------------------------------------------------
-- Electric Units
----------------------------------------------------------------
type VoltUnit = Value ElectricPotential '[ '(Volt, POne)]

type VoltPerCm = Value 
  '[ '(Current, NOne), '(Mass, POne), '(Length, POne), '(Time, NThree) ]
  '[ '(Volt, POne) , '(Centi Meter, NOne)]
type KVPerCm = Value 
  '[ '(Current, NOne), '(Mass, POne), '(Length, POne), '(Time, NThree) ]
  '[ '(Kilo Volt, POne) , '(Centi Meter, NOne)]


type CoulombPerCm2 = 
  Value
    '[ '(Current, POne), '(Length, NTwo), '(Time, POne)]
    '[ '(Ampere, POne), '(Centi Meter, NTwo), '(Second, POne) ] 

type Coulomb = 
  Value
    '[ '(Current, POne), '(Time, POne)]
    '[ '(Ampere, POne), '(Second, POne) ] 

-- eps0
type PermittivityUnit = 
  Value
    '[ '(Mass, NOne),  '(Length, NThree), '(Time, PFour), '(Current, PTwo)]
    '[ '(Kilo Gram, NOne) , '(Meter, NThree), '(Second, PFour), '(Ampere, PTwo)] 

-- mu0
type PermeabilityUnit = 
  Value
    '[ '(Mass, POne),  '(Length, POne), '(Time, NTwo), '(Current, NTwo)]
    '[ '(Kilo Gram, POne) , '(Meter, POne), '(Second, NTwo), '(Ampere, NTwo)] 



  
