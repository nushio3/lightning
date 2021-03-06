module Model.Constants where

import Data.Metrology
import Data.Metrology.Synonyms
import qualified Data.Metrology.NoPrelude as U

electronMass :: Kg Double
electronMass = mkVal 9.10938291e-31

protonMass :: Kg Double
protonMass = mkVal 1.67262178e-27 

solarMass :: Kg Double
solarMass = mkVal 1.98892e30

lightSpeed :: MeterPerSec Double
lightSpeed = U.sqrt $ autoc $ (one :: NoDimension Double) |/| 
     (vacuumPermittivity |*| vacuumPermeability)
    


vacuumPermittivity :: PermittivityUnit Double
vacuumPermittivity = mkVal $ 8.854187817620e-12

vacuumPermeability :: PermeabilityUnit Double
vacuumPermeability = mkVal $ pi * 4e-7

