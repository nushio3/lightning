module Model.Values where

import UnitTyped
import qualified UnitTyped.SI.Constants as C
import UnitTyped.Synonyms

----------------------------------------------------------------
-- Mass units
----------------------------------------------------------------

solarMass :: GramUnit Double
solarMass = mkVal 1.98892e33

earthMass :: GramUnit Double
earthMass = mkVal 5.9742e24

electronMass :: GramUnit Double
electronMass = mkVal 9.10938291e-28

protonMass :: GramUnit Double
protonMass = mkVal 1.67262178e-24


speedOfLight :: CmPerSec Double
speedOfLight = mkVal 29979245800

elementaryCharge :: Coulomb Double
elementaryCharge = mkVal 1.60217657e-19

gravitationalConstant = C.g

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
