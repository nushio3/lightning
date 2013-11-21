{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module UnitTyped.Synonyms where

import           Data.Typeable
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

type HertzUnit =  Value '[ '(Time, NOne)] '[ '(Second, NOne) ] 
type PerSecond =  Value '[ '(Time, NOne)] '[ '(Second, NOne) ] 
type PerCm3 =  Value '[ '(Length, NThree)] '[ '(Centi Meter, NThree) ] 
type PerCm2 =  Value '[ '(Length, NTwo)] '[ '(Centi Meter, NTwo) ] 
type PerCm =  Value '[ '(Length, NOne)] '[ '(Centi Meter, NOne) ] 
type GHz =  Value '[ '(Time, NOne)] '[ '(Giga Hertz, POne) ] 

----------------------------------------------------------------
-- Weighted units
----------------------------------------------------------------

-- densities
type GramPerCm2 =  Value '[ '(Mass, POne),  '(Length, NTwo)] '[ '(Gram, POne), '(Centi Meter, NTwo) ] 
type GramPerCm3 =  Value '[ '(Mass, POne),  '(Length, NThree)] '[ '(Gram, POne), '(Centi Meter, NThree) ] 

type JouleM3 =  Value '[ '(Mass, POne),  '(Length, PFive),  '(Time, NTwo)] '[ '(Joule, POne), '(Meter, PThree) ] 



-- see the table in http://en.wikipedia.org/wiki/Spectral_irradiance
type SpectralRadiance = 
    Value '[ '(Mass, POne), '(Time, NTwo)] '[ '(Watt, POne), '(Meter, NTwo), '(Hertz, NOne) {- Steradian, NOne -}  ] 



-- | Spectral Flux Density
type SpectralFluxDensity = '[ '(Mass, POne), '(Time, NTwo)] 
-- |Unit of EDP
data Jansky
        deriving Typeable

instance Convertible Jansky where
        factor _ = 1e-26
        showunit _ = "Jy"
        type DimensionOf Jansky = SpectralFluxDensity
type JanskyUnit = Value SpectralFluxDensity '[ '(Jansky, POne) ]


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
type Pc = Value '[ '(Length, POne)] '[ '(Parsec, POne) ]

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

-- dipole moment
type DebyeOf = Value ElectricDipoleMoment '[ '(Debye, POne)]

  
