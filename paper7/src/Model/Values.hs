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


hcoMass :: Kg Double
hcoMass = 29 *| protonMass
dcoMass :: Kg Double
dcoMass = 30 *| protonMass
n2hMass :: Kg Double
n2hMass = 29 *| protonMass


elementaryCharge :: Coulomb Double
elementaryCharge = mkVal 1.60217657e-19
