{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Metrology.Synonyms where

import           Data.Metrology
import           Data.Metrology.SI.Poly
import           Data.Metrology.Unsafe
import           Text.Printf
import qualified Data.Metrology.SI.Dims as D

-- My System of Units in this paper
type MySU = SI
type QofU u = MkQu_ULN u MySU Double

----------------------------------------------------------------
-- Pretty Printing Functions
----------------------------------------------------------------

-- TODO: The behavior of ppVal functions are critically different between unittyped and units. This should be fixed.

ppValF :: PrintfArg x => String -> Qu d l x -> String
ppValF fmtStr (Qu x) = printf fmtStr x --  ++ showFactor (Proxy :: Proxy (LookupList dims lcsu)))

ppValFIn :: (Unit u, CompatibleUnit l u) => u -> String -> MkQu_ULN u l Double -> String
ppValFIn u fmtStr x = printf fmtStr (x#u) 

ppValE :: PrintfArg x => Int -> Qu d l x -> String
ppValE d (Qu x) = ret
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
-- Nonweighted units
----------------------------------------------------------------

type PerSecond =  Number :/ Second
type PerCm3 =  (Centi :@ Meter) :^ MThree
type PerCm2 = (Centi :@ Meter) :^ MTwo
type PerCm = (Centi :@ Meter) :^ MOne
type GHz =  (Giga :@ Hertz)

----------------------------------------------------------------
-- Weighted units
----------------------------------------------------------------

-- densities
type GramPerCm2 = Gram :* PerCm2
type GramPerCm3 = Gram :* PerCm3 

type JouleM3 = Joule :/ (Meter :^ Three)


-- see the table in http://en.wikipedia.org/wiki/Spectral_irradiance
type SpectralRadiance = Watt :/ (Meter :^ Two) :/ Hertz


-- | Spectral Flux Density
-- type SpectralFluxDensity = '[ '(Mass, POne), '(Time, NTwo)] 
-- |Unit of EDP
data Jansky = Jansky
instance Show Jansky where show _ = "Jy"

instance Unit Jansky where
  type BaseUnit Jansky = Joule :/ (Meter :^ Two) 
  conversionRatio _ = 1e-26


-- energies
data ElectronVolt = ElectronVolt
instance Show ElectronVolt where show _ = "eV"
instance Unit ElectronVolt where
  type BaseUnit ElectronVolt = Joule 
  conversionRatio _ = 1.60217657e-19


type JouleSecond = Joule :* Second


-- velocities
type KmPerSec = (Kilo :@ Meter) :/ Second
type MPerSec = Meter :/ Second
type CmPerSec = (Centi :@ Meter) :/ Second

type Kg = Kilo :@ Gram

type GramPerMole = Gram :/ Mole


data AU = AU
instance Show AU where show _ = "au"
instance Unit AU where
  type BaseUnit AU = Meter
  conversionRatio _ = 149597870700 

data Parsec = Parsec
instance Show Parsec where show _ = "pc"
instance Unit Parsec where
  type BaseUnit Parsec = Meter
  conversionRatio _ = 3.08567758e16



-- squared velocities
type Cm2PerSec2 = CmPerSec :^ Two
type Meter2PerSec2 = MPerSec :^ Two
type Sec2PerMeter2 = MPerSec :^ MTwo

-- areas
type Meter2 = Meter :^ Two
type Cm2 = (Centi :@ Meter) :^ Two


type SIGCUnit =
  (Meter :^ Three) :* ((Kilo :@ Gram) :^ MOne) :* (Second :^ MTwo)
type SIkBUnit = Joule :/ Kelvin


----------------------------------------------------------------
-- Electric Units
----------------------------------------------------------------
type VoltPerCm = Volt :/ (Centi :@ Meter)
type KVPerCm = (Kilo :@ Volt) :/ (Centi :@ Meter)


type CoulombPerCm2 = Coulomb :/ Cm2

-- eps0
type SIPermittivityUnit = 
  ((Kilo :@ Gram) :^ MOne) :*
  (Meter :^ MThree) :*
  (Second :^ Four) :*
  (Ampere :^ Two)

-- mu0
type SIPermeabilityUnit = 
  ((Kilo :@ Gram) :^ One) :*
  (Meter :^ One) :*
  (Second :^ MTwo) :*
  (Ampere :^ MTwo)


-- dipole moment
data Debye = Debye
instance Unit Debye where
  type BaseUnit Debye = Coulomb :* Meter
  conversionRatio _ = 1e-21/299792458
  
  
{-# LANGUAGE TypeOperators, DataKinds #-}

-----------------------------------------------------------------------------
-- Type synonyms for my system of units, using a Double as the
-- internal representation.
-----------------------------------------------------------------------------





type Length              = MkQu_DLN D.Length              MySU Double
type Mass                = MkQu_DLN D.Mass                MySU Double
type Time                = MkQu_DLN D.Time                MySU Double
type Current             = MkQu_DLN D.Current             MySU Double
type Temperature         = MkQu_DLN D.Temperature         MySU Double
type AmountOfSubstance   = MkQu_DLN D.AmountOfSubstance   MySU Double
type LuminousIntensity   = MkQu_DLN D.LuminousIntensity   MySU Double

type Area                = MkQu_DLN D.Area                MySU Double
type Volume              = MkQu_DLN D.Volume              MySU Double
type Velocity            = MkQu_DLN D.Velocity            MySU Double
type Acceleration        = MkQu_DLN D.Acceleration        MySU Double
type Wavenumber          = MkQu_DLN D.Wavenumber          MySU Double
type Density             = MkQu_DLN D.Density             MySU Double
type SurfaceDensity      = MkQu_DLN D.SurfaceDensity      MySU Double
type SpecificVolume      = MkQu_DLN D.SpecificVolume      MySU Double
type CurrentDensity      = MkQu_DLN D.CurrentDensity      MySU Double
type MagneticStrength    = MkQu_DLN D.MagneticStrength    MySU Double
type Concentration       = MkQu_DLN D.Concentration       MySU Double
type Luminance           = MkQu_DLN D.Luminance           MySU Double
type Frequency           = MkQu_DLN D.Frequency           MySU Double
type Force               = MkQu_DLN D.Force               MySU Double
type Pressure            = MkQu_DLN D.Pressure            MySU Double
type Energy              = MkQu_DLN D.Energy              MySU Double
type Power               = MkQu_DLN D.Power               MySU Double
type Charge              = MkQu_DLN D.Charge              MySU Double
type ElectricPotential   = MkQu_DLN D.ElectricPotential   MySU Double
type Capacitance         = MkQu_DLN D.Capacitance         MySU Double
type Resistance          = MkQu_DLN D.Resistance          MySU Double
type Conductance         = MkQu_DLN D.Conductance         MySU Double
type MagneticFlux        = MkQu_DLN D.MagneticFlux        MySU Double
type MagneticFluxDensity = MkQu_DLN D.MagneticFluxDensity MySU Double
type Inductance          = MkQu_DLN D.Inductance          MySU Double
type LuminousFlux        = MkQu_DLN D.LuminousFlux        MySU Double
type Illuminance         = MkQu_DLN D.Illuminance         MySU Double
type Kerma               = MkQu_DLN D.Kerma               MySU Double
type CatalyticActivity   = MkQu_DLN D.CatalyticActivity   MySU Double
type Momentum            = MkQu_DLN D.Momentum            MySU Double

