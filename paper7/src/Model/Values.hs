{-# LANGUAGE ConstraintKinds, TypeOperators #-}
module Model.Values where

import Data.Metrology
import Data.Metrology.Synonyms
import Data.Metrology.SI.Units
import Data.Metrology.Z

----------------------------------------------------------------
-- Mass units
----------------------------------------------------------------

solarMass :: Mass
solarMass = 1.98892e33 % Gram

earthMass :: Mass
earthMass = 5.9742e24 % Gram

electronMass :: Mass
electronMass = 9.10938291e-28 % Gram

protonMass :: Mass
protonMass = 1.67262178e-24 % Gram


speedOfLight :: Velocity
speedOfLight =  299792458 % (Meter :/ Second)


elementaryCharge :: Charge
elementaryCharge = 1.60217657e-19 % Coulomb


gravitationalConstant :: QofU SIGCUnit 
gravitationalConstant = 6.67384e-11 % (undefined :: SIGCUnit)

kB :: QofU SIkBUnit 
kB = 1.3806488e-23 % (undefined :: SIkBUnit)


-- eps0
vacuumPermittivity :: QofU SIPermittivityUnit 
vacuumPermittivity =  
  (1 / (4 * pi * 1e-7 * 299792458**2)) % (undefined :: SIPermittivityUnit)
-- mu0                     
vacuumPermeability :: QofU SIPermeabilityUnit 
vacuumPermeability = (4 * pi * 1e-7) % (undefined :: SIPermeabilityUnit)

-- |Planck constant
planckConstant :: QofU JouleSecond 
planckConstant =  (6.6260695729e-34) % (undefined :: JouleSecond)

-- |Reduced Planck constant
hbar :: QofU JouleSecond
hbar = (1 / 2 / pi) *| planckConstant

avogadroConstant :: QofU (Number :/ Mole)
avogadroConstant = 6.02214129e23 % (Number :/ Mole)