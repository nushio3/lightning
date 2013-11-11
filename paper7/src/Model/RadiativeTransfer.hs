{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}


module Model.RadiativeTransfer where

import           Data.Reflection.Typed
import UnitTyped
import qualified UnitTyped.NoPrelude as U
import UnitTyped.Synonyms

import           Model.Concepts
import Model.Gas
import Model.Values
import Model.Disk.Hayashi as MMSN

import           Text.Authoring
import           Text.Authoring.TH

-- | From Alma OT
dipoleMoment :: ChemicalSpecies -> DebyeOf Double 
dipoleMoment N2HPlus = mkVal $ sqrt 11.56 
dipoleMoment HCOPlus = mkVal $ sqrt 15.21
dipoleMoment DCOPlus = mkVal $ sqrt 15.21
dipoleMoment _ = mkVal 0

-- | From WMN 2010
columnDensity100au :: ChemicalSpecies -> PerCm2 Double 
columnDensity100au H2 = mkVal 2.0e23
columnDensity100au x  = columnDensity100au H2 |*| fractionalAbundance100au x

fractionalAbundance100au :: ChemicalSpecies -> NoDimension Double 
fractionalAbundance100au N2HPlus = mkVal $ (5.5e11 / 2.0e23)
fractionalAbundance100au HCOPlus = mkVal $ (4.3e13 / 2.0e23)
fractionalAbundance100au DCOPlus = 0.00194e-2 *| fractionalAbundance100au HCOPlus
fractionalAbundance100au _ = mkVal 0


aboutFractionalAbundance :: MonadAuthoring s w m => m ()
aboutFractionalAbundance = do  
  [rawQ|  
   We adopt the XR+UV-new chemical process model of @{citet ["doi:10.1088/0004-637X/747/2/114"]},
   and assume that the fractional abundances of $\rm HCO^{+}$ and $\rm N_2H^{+}$ at 100au
   are $#{ppValE 0 $ fractionalAbundance100au HCOPlus} $
   and $#{ppValE 0 $ fractionalAbundance100au N2HPlus} $,
   respectively. 
   We assume the fractional abundance of $\rm DCO^{+}$ to be
   $#{ppValE 0 $ fractionalAbundance100au DCOPlus}$, based on the
   hydrogen isotope ratio data in
   @{citet["bibcode:2009LanB...4B...44L"]}.

   Therefore, the column densities of $\rm HCO^{+}$, $\rm DCO^{+}$ and $\rm N_2H^{+}$
   are $#{ppValE 0 $ columnDensity100au HCOPlus} {\rm cm^{ -2}}$,
   $#{ppValE 0 $ columnDensity100au DCOPlus} {\rm cm^{ -2}}$,
   and $#{ppValE 0 $ columnDensity100au N2HPlus} {\rm cm^{ -2}}$,
   respectively.
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
  scovilleFormulaAthermal j tex vgas chem 
  where
    vgas :: CmPerSec Double
    vgas = U.sqrt $ autoc $ exitationNrg |/| molecularMass chem

    exitationNrg :: JouleUnit Double
    exitationNrg = autoc $ kB |*| tex

    
scovilleFormulaAthermal :: Int -> KelvinUnit Double -> CmPerSec Double -> ChemicalSpecies -> PerCm2 Double
scovilleFormulaAthermal j tex vgas chem = 
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


lineRadiation :: Int -> KelvinUnit Double -> ChemicalSpecies -> SpectralRadiance Double
lineRadiation j tem chem = (1 - exp (negate tau)) *| bbr
  where
    nu = lineFrequency j chem
    bbr = blackBodyRadiation (autoc nu) tem
    
    n1 :: PerCm2 Double
    n1 = scovilleFormulaAthermal j tem ligVel chem

    tau :: Double
    tau = val $ columnDensity100au chem |/| n1
    
    ligVel :: CmPerSec Double
    ligVel = mkVal 7e5
    

    

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
   We consider $\rm HCO^{+}~3-2$, $\rm DCO^{+}~3-2$ and  $\rm N_2H^{+}~3-2$ lines.
   Their frequencies are                               
   $#{ppValF "%5.2f" (lineFrequency 2 HCOPlus)}$GHz,
   $#{ppValF "%5.2f" (lineFrequency 2 DCOPlus)}$GHz and
   $#{ppValF "%5.2f" (lineFrequency 2 N2HPlus)}$GHz, respectively.
   At 100au of the MMSN disk $T = #{ppValF "%3.0f" tem100au} {\rm K}$.                                                     
   Therefore, $N_{\tau_{\nu 0}=1}$ for the three lines are
   $#{ppValE 2 $ scovilleFormula 2 tem100au HCOPlus} ~{\rm cm^{ -2}}$,
   $#{ppValE 2 $ scovilleFormula 2 tem100au DCOPlus} ~{\rm cm^{ -2}}$ and   
   $#{ppValE 2 $ scovilleFormula 2 tem100au N2HPlus} ~{\rm cm^{ -2}}$, respectively,
   given no lightning and that the molecules are in their thermal velocities.
   
   On the other hand, $N_{\tau_{\nu 0}=1}$ for the three lines are
   $#{ppValE 2 $ scovilleFormulaAthermal 2 tem100au ligVel HCOPlus} ~{\rm cm^{ -2}}$,
   $#{ppValE 2 $ scovilleFormulaAthermal 2 tem100au ligVel DCOPlus} ~{\rm cm^{ -2}}$ and   
   $#{ppValE 2 $ scovilleFormulaAthermal 2 tem100au ligVel N2HPlus} ~{\rm cm^{ -2}}$, respectively,
   if the molecules are accelerated by the lightning electric field.


   @{aboutFractionalAbundance}


   For a disk 100pc distant from the Earth, and lightning area of $10000 ~ {\rm au}^2$,
   the spectral flux density of the signals are
   $#{ppValE 1 $ lr HCOPlus} {\rm Jy}$,
   $#{ppValE 1 $ lr DCOPlus} {\rm Jy}$ and
   $#{ppValE 1 $ lr N2HPlus} {\rm Jy}$ for
   $\rm HCO^{+}~3-2$, $\rm DCO^{+}~3-2$ and  $\rm N_2H^{+}~3-2$ lines, respectively.

   |]
   where
     tem100au :: KelvinUnit Double
     tem100au = OrbitalRadius `being` (mkVal 100) $ MMSN.temperature

     ligVel :: CmPerSec Double
     ligVel = mkVal 7e5
     
     solidAngle :: Double
     solidAngle = 206264 ** (-2)


     lr :: ChemicalSpecies -> JanskyUnit Double
     lr chem = autoc $ solidAngle *| lineRadiation 2 tem100au chem