{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}


module Model.RadiativeTransfer where

import UnitTyped
import qualified UnitTyped.NoPrelude as U
import UnitTyped.Synonyms

import Model.Gas
import Model.Values

import           Text.Authoring
import           Text.Authoring.TH

-- | From Alma OT
dipoleMoment :: ChemicalSpecies -> DebyeOf Double 
dipoleMoment N2HPlus = mkVal $ sqrt 11.56 
dipoleMoment HCOPlus = mkVal $ sqrt 15.21
dipoleMoment DCOPlus = mkVal $ sqrt 15.21
dipoleMoment _ = mkVal 0

-- | From WMN 2010
columnDensitySpec :: ChemicalSpecies -> PerCm2 Double 
columnDensitySpec N2HPlus = 6e11
columnDensitySpec HCOPlus = 4.7e13
columnDensitySpec DCOPlus = 0.00194e-2 * columnDensitySpec HCOPlus
columnDensitySpec _ = 0

aboutFractionalAbundance :: MonadAuthoring s w m => m ()
aboutFractionalAbundance = do  
  [rawQ|  
   The column densities of $\rm HCO^{+}$ and $\rm DCO^{+}~3-2$ 
   are $#{ppValE 0 $ columnDensitySpec HCOPlus}$
  $\rm N2H^{+}~3-2$ lines.
   |]


rotationalConst :: ChemicalSpecies -> PerSecond Double 
rotationalConst N2HPlus = autoc $ speedOfLight |*| (mkVal 3.1079 :: PerCm Double)
rotationalConst HCOPlus = autoc $ speedOfLight |*| (mkVal 2.9750 :: PerCm Double)
rotationalConst DCOPlus = autoc $ speedOfLight |*| (mkVal 2.4030 :: PerCm Double)
rotationalConst _ = mkVal 0

lineFrequency :: Int -> ChemicalSpecies -> GHz Double 
lineFrequency j chem = autoc $ (1+fromIntegral j :: Double) *| rotationalConst chem


aboutScovilleFormula :: MonadAuthoring s w m => m ()
aboutScovilleFormula = 
  environment "eqnarray" $ [rawQ|
N_{\tau_{\nu 0}=1} = 
\frac{3 \epsilon_0 k_B T_{\rm ex} \Delta v_{\rm gas}} {2 \pi^2 B \mu^2 \cos \theta}
\frac{1}{(J+1)}
\exp
\left(
  \frac{hBJ(J+1)}{k_B T_{\rm ex}}
\right)
\left(
  1 - \exp 
  \left(
    - \frac{h \nu_0}{k_B T_{\rm ex}}
  \right)       
\right),
        |]


scovilleFormula :: Int -> KelvinUnit Double -> ChemicalSpecies -> PerCm2 Double
scovilleFormula j tex chem = 
  autoc $ 
    factor *|
    (exitationNrg |*| vgas) |/| 
    (rotB |*| dipoleInteraction)
  where
    factor :: Double
    factor = (3 / (2*pi*pi)) / (realJ+1) * exp(val firstExp)  * (1 - exp(- val secondExp))
    
    firstExp :: NoDimension Double
    firstExp = (realJ * (realJ+1)) *| (planckConstant |*| rotB)  |/| (exitationNrg)
    
    secondExp :: NoDimension Double
    secondExp = ((2*realJ) *| planckConstant |*| rotB)  |/| (exitationNrg)
    
    realJ :: Double
    realJ = fromIntegral j
    
    rotB :: PerSecond Double
    rotB = rotationalConst chem
    
    exitationNrg :: JouleUnit Double
    exitationNrg = autoc $ kB |*| tex

    vgas :: MeterPerSec Double
    vgas = U.sqrt $ autoc $ exitationNrg |/| molecularMass chem

    deb :: DebyeOf Double
    deb = dipoleMoment chem
    
    dipoleInteraction :: JouleM3 Double
    dipoleInteraction = autoc $  (deb |*| deb) |/| vacuumPermittivity

blackBodyRadiation :: PerSecond Double -> KelvinUnit Double -> SpectralRadiance Double
blackBodyRadiation nu tem = autoc $ 
  factor *| planckConstant |*| cubic nu |/| square speedOfLight
    where
      factor :: Double
      factor = 2 / (exp (val ie) - 1)
      
      ie :: NoDimension Double
      ie = (planckConstant |*| nu) |/| (kB |*| tem)


lineRadiation :: Int -> KelvinUnit Double -> PerCm2 Double -> ChemicalSpecies -> SpectralRadiance Double
lineRadiation j tem colDensHydrogen chem = undefined


-- http://www.cv.nrao.edu/course/astr534/Equations.html

aboutScoville :: MonadAuthoring s w m => m ()
aboutScoville = do
  [rawQ|
  The column density corresponding to optical depth $\tau_{\nu 0}=1$ is estimated as follows
  @{citationGen "citep[see the appendix of][]" ["bibcode:1986ApJ...303..416S"]}

  @{aboutScovilleFormula} 

  where $B$ is the rotational constant of the molecule, $\mu$ is its electric dipole matrix element,
  $h \nu_0$ is the energy difference between the two levels 
  and $J$ is the rotational quantum number of the lower state.

  The optical depth of the disk with column density $N$ is

\begin{eqnarray}
\tau_{\nu_0}(N) = N / N_{\tau_{\nu 0}=1} ,
\end{eqnarray}


  so that the intensity is

\begin{eqnarray}
I(\nu_0) = B(\nu_0,T) \left(1-\exp (-\tau_{\nu_0}(N))\right)   .
\end{eqnarray}

 
  Here $ B(\nu_0,T) $ is the vacuum brightness of a black body at frequency $\nu_0$
  (see @{citationGen "citet[chap. 2.7]" ["isbn:3-540-29692-1"]}). 

  |]


aboutLineObservation :: MonadAuthoring s w m => m ()
aboutLineObservation = do
  [rawQ|
   We consider $\rm HCO^{+}~3-2$, $\rm DCO^{+}~3-2$ and  $\rm N2H^{+}~3-2$ lines.
   Their frequencies are                               
   $#{ppValF "%5.2f" (lineFrequency 2 HCOPlus)}$GHz,
   $#{ppValF "%5.2f" (lineFrequency 2 DCOPlus)}$GHz and
   $#{ppValF "%5.2f" (lineFrequency 2 N2HPlus)}$GHz, respectively.
                                                     
                                                     
   $#{ppValE 2 $ scovilleFormula 2 (mkVal 15) HCOPlus}$,
   $#{ppValE 2 $ scovilleFormula 2 (mkVal 15) DCOPlus}$ and   
   $#{ppValE 2 $ scovilleFormula 2 (mkVal 15) N2HPlus}$,
   |]
    