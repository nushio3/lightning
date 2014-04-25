{-# LANGUAGE ConstraintKinds, TypeOperators #-}
module Model.Values where

import Data.Metrology
import Data.Metrology.Synonyms
import Data.Metrology.SI.Units
import qualified Data.Metrology.SI.Dims as D
import Data.Metrology.SI.GenTypes
import Data.Metrology.Z

----------------------------------------------------------------
-- Mass units
----------------------------------------------------------------

solarMass :: Mass MySU Double
solarMass = 1.98892e33 % Gram

earthMass :: Mass MySU Double
earthMass = 5.9742e24 % Gram

electronMass :: Mass MySU Double
electronMass = 9.10938291e-28 % Gram

protonMass :: Mass MySU Double
protonMass = 1.67262178e-24 % Gram


speedOfLight :: Velocity MySU Double
speedOfLight =  299792458 % (Meter :/ Second)


elementaryCharge :: Charge MySU Double
elementaryCharge = 1.60217657e-19 % Coulomb


gravitationalConstant :: QuOfUL SIGCUnit MySU
gravitationalConstant = 6.67384e-11 % (undefined :: SIGCUnit)

kB :: QuOfUL SIkBUnit MySU
kB = 1.3806488e-23 % (undefined :: SIkBUnit)


planckLength :: Length MySU Double
planckLength = qSqrt (hbar |*| gravitationalConstant |/| (speedOfLight |^ pThree))

-- eps0
vacuumPermittivity :: QuOfUL SIPermittivityUnit MySU
vacuumPermittivity =  
  (1 / (4 * pi * 1e-7 * 299792458**2)) % (undefined :: SIPermittivityUnit)
-- mu0                     
vacuumPermeability :: QuOfUL SIPermeabilityUnit MySU
vacuumPermeability = (4 * pi * 1e-7) % (undefined :: SIPermeabilityUnit)

-- |Planck constant
planckConstant :: QuOfUL JouleSecond MySU
planckConstant =  (6.6260695729e-34) % (undefined :: JouleSecond)

-- |Reduced Planck constant
hbar :: QuOfUL JouleSecond MySU
hbar = (1 / 2 / pi) *| planckConstant

avogadroConstant :: MkGenQu (D.AmountOfSubstance :^ MOne) MySU Double
avogadroConstant = 6.02214129e23 % (Number :/ Mole)