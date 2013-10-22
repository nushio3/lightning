{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Gas where

import Control.Applicative
import Text.Authoring
import Text.Authoring.TH
import UnitTyped
import UnitTyped.Synonyms

import Model.Breakdown (ntpAirDielectricStrength)
import Model.Values

data ChemicalSpecies
  = H2
  | N2
  | O2
  | H2O
  | Ar
  | He
  | HCOPlus
  | DCOPlus
  | N2HPlus
    
airMix :: Convertible' a b => (ChemicalSpecies -> Value a b Double) -> Value a b Double
airMix func = 0.78 *| func N2 |+| 0.21 *| func O2 |+| 0.01 *| func Ar

ppdMix :: Convertible' a b => (ChemicalSpecies -> Value a b Double) -> Value a b Double
ppdMix func = 0.9105 *| func H2 |+| 8.8769e-2 *| func He |+| 7.7662e-4 *| func O2

atomicNumber :: ChemicalSpecies -> NoDimension Double 
atomicNumber H2 = mkVal 2
atomicNumber He = mkVal 2
atomicNumber N2 = mkVal 14
atomicNumber O2 = mkVal 16
atomicNumber Ar = mkVal 18
atomicNumber H2O = mkVal 10
atomicNumber HCOPlus = mkVal 15
atomicNumber DCOPlus = mkVal 15
atomicNumber N2HPlus = mkVal 15



molecularMass :: ChemicalSpecies -> GramPerMole Double 
molecularMass H2 = mkVal 2
molecularMass He = mkVal 4
molecularMass N2 = mkVal 28
molecularMass O2 = mkVal 32
molecularMass Ar = mkVal 39.9
molecularMass H2O = mkVal 18
molecularMass HCOPlus = mkVal 29
molecularMass DCOPlus = mkVal 30
molecularMass N2HPlus = mkVal 29

ionizationEnergy :: ChemicalSpecies -> ElectronVolt Double
ionizationEnergy = go
  where
    go H2  = mkVal 15.43
    go N2  = mkVal 15.58
    go O2  = mkVal 12.07
    go H2O = mkVal 12.61
    go Ar  = mkVal 15.76
    go He  = mkVal 24.58
    go _   = undefined

inelCrossSection :: Int -> ChemicalSpecies -> Cm2 Double    
inelCrossSection 12 N2 = mkVal 0.8e-16
inelCrossSection 12 O2 = mkVal 1.8e-16
inelCrossSection 15 O2 = mkVal 1.81e-16
inelCrossSection 12 Ar = mkVal 0
inelCrossSection 15 H2 = mkVal 1.57e-16
inelCrossSection _  _  = mkVal 0

mfpAir12 :: Cm Double
mfpAir12 = autoc $ 1 /| airNumberDensity |/| (airMix $ inelCrossSection 12)


   

airDielectricStrength :: KVPerCm Double
airDielectricStrength = autoc $ w |/| (mfpAir12 |*| elementaryCharge)
  where w = mkVal 12 :: ElectronVolt Double
                        
airDielectricStrengthDP :: KVPerCm Double
airDielectricStrengthDP = autoc $ ratio *| w |/| (0.43 *| elementaryCharge |*| mfpAir12) 
  where
    w = mkVal 12                           :: ElectronVolt Double
    ratio = sqrt $ val ratioD              :: Double
    ratioD = autoc $ electronMass |/| bigM :: NoDimension Double
    bigM = autoc $ airMix molecularMass    :: GramUnit Double

airDielectricStrengthR :: KVPerCm Double
airDielectricStrengthR = autoc $ 
  (11/(4*pi)) *| (e3 |*| z |*| airNumberDensity)
             |/| (vacuumPermittivity |*| vacuumPermittivity |*| nrg)
  
  where
    nrg :: JouleUnit Double
    nrg = autoc $ electronMass |*| speedOfLight |*| speedOfLight
    
    e3 = elementaryCharge |*| elementaryCharge |*| elementaryCharge 
    z = airMix atomicNumber
    n = airNumberDensity


airDensity :: GramPerCm3 Double
airDensity = mkVal 1.2041e-3

airNumberDensity :: PerCm3 Double
airNumberDensity = autoc $ airDensity |/| airMix molecularMass

aboutAir :: MonadAuthoring s w m => m ()
aboutAir = do
  [rawQ|
We assume that air consists of
78\% $\rm N_2$,  21\% $\rm O_2$, and 1\% $\rm Ar$ (volume fraction).
Air number density at NTP is $#{ppValE 3 airNumberDensity} {\rm cm^{ -3}}$ .
The ionization energy of these chemical species are
$\Delta W_{\rm N_2} = #{val $ ionizationEnergy N2} {\rm eV}$,
$\Delta W_{\rm O_2} = #{val $ ionizationEnergy O2} {\rm eV}$, and
$\Delta W_{\rm Ar}  = #{val $ ionizationEnergy Ar} {\rm eV}$, respectively.
Of these $\Delta W_{\rm O_2} \sim 12 {\rm eV}$ is the smallest, so we estimate
the electric field amplitude $E_{\rm crit}$ required to accelerate the electron
upto 12eV; i.e. we solve $12 {\rm eV} = e E_{\rm crit} l_{\rm mfp}$.
Given that
the inelastic cross sections of $\rm N_2, O_2, Ar$ for 12 eV electron 12eV are
$0.8, ~1.8, ~0.0 \times 10^{ -16} {\rm cm^{ -2}}$
@{citep ["isbn:3-540-64296-X", "isbn:354044338X"]},
the mean inelastic cross section of air at 12eV is 
$#{ppValE 1 $ airMix $ inelCrossSection 12} {\rm cm^{ -2}}$.
Therefore, $l_{\rm mfp} = #{ppValE 1 mfpAir12} {\rm cm}$.
This gives 
$E_{\rm crit} = #{ppValF "%.0f" airDielectricStrength} {\rm kV/cm}$,
 which is in agreement with the dielectric strength of air at ground level
(Equations (@{ref ntpAirDielectricStrength})).

On the other hand, according to the formalization by
@{citet ["doi:10.1086/432796"]}, average kinetic energy of electron
under the electric field $E$ is
\begin{eqnarray}
  \langle \epsilon \rangle = 0.43 e E l_{\rm mfp} \sqrt{\frac{M}{m_e}},
\end{eqnarray}
where $M$ is the mass of the collision partner, 
and the dielectric strength $E_{\rm crit}$ is the solution of $\langle \epsilon \rangle = \Delta W$.
In the case of the air at NTP,
since mean molecular weight of air is #{ppValF "%0.2f" $ airMix molecularMass},
$E_{\rm crit} = #{ppValF "%.1f" airDielectricStrengthDP} {\rm kV/cm}$.

Finally, according to the runaway breakdown model,
$E_{\rm crit} = #{ppValF "%.1f" airDielectricStrengthR} {\rm kV/cm}$
@{citep ["isbn:3-540-29692-1"]}.
   |]


