module Model.Values where

import UnitTyped
import UnitTyped.Synonyms

----------------------------------------------------------------
-- Mass units
----------------------------------------------------------------

solarMass :: Kg Double
solarMass = mkVal 1.98892e33

earthMass :: Kg Double
earthMass = mkVal 5.9742e24

electronMass :: Kg Double
electronMass = mkVal 9.10938291e-31

protonMass :: Kg Double
protonMass = mkVal 1.67262178e-27


speedOfLight :: CmPerSec Double
speedOfLight = mkVal 29979245800

elementaryCharge :: Coulomb Double
elementaryCharge = mkVal 1.60217657e-19



-- eps0
vacuumPermittivity :: PermittivityUnit Double
vacuumPermittivity = mkVal $ 1 / (4 * pi * 1e-7 * 299792458**2)
  
-- mu0                     
vacuumPermeability :: PermeabilityUnit Double
vacuumPermeability = mkVal $ 4 * pi * 1e-7