{-# LANGUAGE ConstraintKinds #-}
module Model.Values where

import Data.Metrology
import Data.Metrology.Synonyms
import Data.Metrology.SI.Units
import Data.Metrology.SI.GenTypes

----------------------------------------------------------------
-- Mass units
----------------------------------------------------------------

solarMass :: Compatible l Gram => Mass l Double
solarMass = 1.98892e33 % Gram

earthMass :: Compatible l Gram => Mass l Double
earthMass = 5.9742e24 % Gram

electronMass :: Compatible l Gram => Mass l Double
electronMass = 9.10938291e-28 % Gram

protonMass :: Compatible l Gram => Mass l Double
protonMass = 1.67262178e-24 % Gram


speedOfLight :: Compatible l CmPerSec => Velocity l Double
speedOfLight =  29979245800 % (undefined :: CmPerSec)


elementaryCharge :: (Compatible l Coulomb) => Charge l Double
elementaryCharge = 1.60217657e-19 % Coulomb


gravitationalConstant :: (Compatible l GravitationalConstantUnit) 
  => MkGenQu (DimFactorsOf GravitationalConstantUnit) l Double
gravitationalConstant = 6.67384e-11 % (undefined :: GravitationalConstantUnit)

{-
kB = C.kB


-- eps0
vacuumPermittivity :: PermittivityUnit Double
vacuumPermittivity = mkVal $ 1 / (4 * pi * 1e-7 * 299792458**2)
  
-- mu0                     
vacuumPermeability :: PermeabilityUnit Double
vacuumPermeability = mkVal $ 4 * pi * 1e-7

-- |Planck constant
planckConstant :: JouleSecond Double
planckConstant = mkVal 6.6260695729e-34

-- |Reduced Planck constant
hbar ::  JouleSecond Double
hbar = mkVal $ 6.6260695729e-34 / 2 / pi
-}