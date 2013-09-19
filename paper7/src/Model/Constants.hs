module Model.Constants where

import UnitTyped
import UnitTyped.Synonyms


electronMass :: Kg Double
electronMass = mkVal 9.10938291e-31

protonMass :: Kg Double
protonMass = mkVal 1.67262178e-27 

solarMass :: Kg Double
solarMass = mkVal 1.98892e30

vacuumPermeability :: PermeabilityUnit Double
vacuumPermeability = mkVal $ pi * 4e-7