{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}


module Model.RadiativeTransfer where

import Control.Lens hiding ((#))
import Control.Monad
import Data.Metrology
import Data.Metrology.SI.Units
import Data.Metrology.SI.Prefixes
import Data.Metrology.Synonyms

import Model.Gas
import Model.Values
import Model.Disk
import Model.Disk.Hayashi 

import           Text.Authoring
import           Text.Authoring.TH

fieldToVelocity :: Environment -> ChemicalSpecies -> Velocity
fieldToVelocity env chem = qSqrt $ v2
  where                     
    ef :: QofU VoltPerCm 
    ef = env ^. lightningAccelerator 
    v2 :: QofU Cm2PerSec2 
    v2 = redim $ elementaryCharge |*| ef |*| (env^.mfpPpd15) |/| m 
    m :: Mass
    m = molecularMass chem |/| avogadroConstant



aboutLineObservation :: MonadAuthoring s w m => m ()
aboutLineObservation = do
  aboutScoville

  raw "\n\n"

  aboutLineProperty

  raw "\n\n"
  
  aboutLineProfile


-- | From Alma OT
dipoleMoment :: ChemicalSpecies -> QofU Debye
dipoleMoment N2HPlus =  11.56 % Debye
dipoleMoment HCOPlus =  15.21 % Debye
dipoleMoment DCOPlus =  15.21 % Debye
dipoleMoment _ = zero

-- | From WMN 2010
columnDensity100au :: ChemicalSpecies -> QofU PerCm2
columnDensity100au H2 = 2.0e23 % (undefined :: PerCm2)
columnDensity100au x  = columnDensity100au H2 |*| fractionalAbundance100au x

fractionalAbundance100au :: ChemicalSpecies -> QofU Number
fractionalAbundance100au N2HPlus = scalar $ 5.3e-10 --(5.5e11 / 2.0e23)
fractionalAbundance100au HCOPlus = scalar $ 9.0e-9 --(4.3e13 / 2.0e23)
fractionalAbundance100au DCOPlus = 0.3 *| fractionalAbundance100au HCOPlus
fractionalAbundance100au _ = zero


aboutFractionalAbundance :: MonadAuthoring s w m => m ()
aboutFractionalAbundance = do  
  [rawQ|  
   For simplicity we assume that fractional abundances of the ion species are uniform
   within the disk.
   We adopt the values at 100au of the XR+UV-new chemical process model 
   @{citep ["doi:10.1088/0004-637X/747/2/114"]},
   and assume that the fractional abundances of $\rm HCO^{+}$ and $\rm N_2H^{+}$ 
   are $#{ppValE 0 $ fractionalAbundance100au HCOPlus} $
   and $#{ppValE 0 $ fractionalAbundance100au N2HPlus} $,
   respectively. 
  |]

  when False $ [rawQ|  
   We assume the fractional abundance of $\rm DCO^{+}$ to be
   $#{ppValE 0 $ fractionalAbundance100au DCOPlus}$, based on the
   hydrogen isotope ratio data in
   @{citet["bibcode:2009LanB...4B...44L"]}. The new paper by
   @{citet["bibcode:2013A&A...557A.132M"]} reports enhancement of DCO.
   |]


  [rawQ|  
   We assume the fractional abundance of $\rm DCO^{+}$ to be
   $#{ppValE 0 $ fractionalAbundance100au DCOPlus}$, based on the
   observations by @{citet["bibcode:2013A&A...557A.132M"]}.
   |]
  

  [rawQ|  
   Therefore, the column densities of $\rm HCO^{+}$, $\rm DCO^{+}$ and $\rm N_2H^{+}$
   are $#{ppValE 0 $ columnDensity100au HCOPlus} {\rm cm^{ -2}}$,
   $#{ppValE 0 $ columnDensity100au DCOPlus} {\rm cm^{ -2}}$,
   and $#{ppValE 0 $ columnDensity100au N2HPlus} {\rm cm^{ -2}}$,
   respectively.
   |]


rotationalConst :: ChemicalSpecies -> Frequency
rotationalConst N2HPlus =  speedOfLight |*| (3.1079 % (Number :/ centi Meter))
rotationalConst HCOPlus =  speedOfLight |*| (2.9750 % (Number :/ centi Meter))
rotationalConst DCOPlus =  speedOfLight |*| (2.4030 % (Number :/ centi Meter))
rotationalConst _ = zero

lineFrequency :: Int -> ChemicalSpecies -> Frequency
lineFrequency j chem = (1+fromIntegral j :: Double) *| rotationalConst chem


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


scovilleFormula :: Int -> Temperature -> ChemicalSpecies -> QofU PerCm2 
scovilleFormula j tex chem = 
  scovilleFormulaAthermal j tex vgas chem 
  where
    vgas :: Velocity
    vgas = redim $ qSqrt $ exitationNrg |/| (molecularMass chem |/| avogadroConstant)

    exitationNrg :: Energy
    exitationNrg =  kB |*| tex

    
scovilleFormulaAthermal :: Int -> Temperature -> Velocity -> ChemicalSpecies -> QofU PerCm2 
scovilleFormulaAthermal j tex vgas chem = redim $ 
    factor *|
    (exitationNrg |*| vgas) |/| 
    (rotB |*| dipoleInteraction)
  where
    factor :: Double
    factor = (3 / (2*pi*pi)) / (realJ+1) * exp(firstExp # Number)  * (1 - exp(- (secondExp#Number)))
    
    firstExp :: QofU Number
    firstExp = (realJ * (realJ+1)) *| (planckConstant |*| rotB)  |/| (exitationNrg)
    
    secondExp :: QofU Number
    secondExp = ((2*realJ) *| planckConstant |*| rotB)  |/| (exitationNrg)
    
    realJ :: Double
    realJ = fromIntegral j
    
    rotB :: Frequency
    rotB = rotationalConst chem
    
    exitationNrg :: Energy
    exitationNrg = kB |*| tex

    deb :: QofU Debye
    deb = dipoleMoment chem
    
    dipoleInteraction = (deb |*| deb) |/| vacuumPermittivity

blackBodyRadiation :: Frequency -> Temperature -> QofU SpectralRadiance 
blackBodyRadiation nu tem = redim $ 
  factor *| planckConstant |*| qCube nu |/| qSq speedOfLight
    where
      factor :: Double
      factor = 2 / (exp (ie) - 1)
      
      ie :: Double
      ie = (planckConstant |*| nu) |/| (kB |*| tem) # Number


lineRadiation100au :: Int -> Temperature -> ChemicalSpecies -> QofU SpectralRadiance 
lineRadiation100au j tem chem = (1 - exp (negate tau)) *| bbr
  where
    nu = lineFrequency j chem
    bbr = blackBodyRadiation nu tem
    
    n1 :: QofU PerCm2
    n1 = scovilleFormulaAthermal j tem ligVel chem

    tau :: Double
    tau = columnDensity100au chem |/| n1 # Number
    
    ligVel :: Velocity
    ligVel = 7e5 % (undefined :: CmPerSec )
    

lineRadiation :: QofU PerCm2  -> Velocity -> Int -> Temperature -> ChemicalSpecies -> QofU SpectralRadiance 
lineRadiation colDens ligVel j tem chem = (1 - exp (negate tau)) *| bbr
  where
    nu = lineFrequency j chem
    bbr = blackBodyRadiation nu tem
    
    n1 :: QofU PerCm2
    n1 = scovilleFormulaAthermal j tem ligVel chem

    tau :: Double
    tau = colDens |/| n1 # Number
    
    

    



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


aboutLineProperty :: MonadAuthoring s w m => m ()
aboutLineProperty = do
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
   We can see that the disk is optically thick for
   $\rm HCO^{+}~3-2$ line at 100au. On the other hand, it is optically thin for 
   $\rm DCO^{+}~3-2$ and  $\rm N_2H^{+}~3-2$ lines. Once lightning takes place,
   however, all the three lines becomes optically thin because molecular speed becomes faster. 
   Observational consequence of this are brighter and broadened line profiles.
   

   |]
   where
     tem100au :: Temperature
     tem100au = view temperature (mmsnModel >$< equatorAt (100 % AU)) 

     ligVel :: Velocity
     ligVel = 7e5 % (undefined :: CmPerSec)
     
     solidAngle :: Double
     solidAngle = 206264 ** (-2)


     lr :: ChemicalSpecies -> QofU Jansky
     lr chem = redim $ solidAngle *| lineRadiation100au 2 tem100au chem

aboutLineProfile :: MonadAuthoring s w m => m ()
aboutLineProfile = do
  [rawQ|    
   
The spectral irradiance of the disk $E(\nu)$
as a function of $\nu$ is
   
\begin{eqnarray}   
E(\nu) &=& \frac{1}{D^2}
\int \int I(\nu_0,r) \exp
\left(
- \frac{m c^2 d(\nu; \nu_0,r)^2}{2 k_B T {\nu_0}^2}
\right) 
r \mathit{dr} \mathit{d\varphi} \cos i ,\\
\mathrm {where} ~~~
d(\nu; \nu_0,r) &=& \nu - \nu_0 - \frac{v_K(r)}{c} \cos \varphi \sin i \nonumber .
\end{eqnarray}   
   
   |]

lineProfile :: Disk -> Int -> ChemicalSpecies -> (Velocity -> QofU Jansky)
lineProfile disk j chem dv = foldl1 (|+|) $ map go splittedDisk
  where
    go :: DiskPortion -> QofU Jansky
    go (DiskPortion pos a0) = redim $ (exp $ negate $ expPart) *| peakRadiance |*| a0 |/| qSq (env's distanceFromEarth)
      where
        env = disk >$< pos
        env's = (env ^.)        

        phi :: Double
        phi = env's azimuth
        
        incli :: Double
        incli = env's inclinationAngle 
        
        expPart :: Double
        expPart =  (#Number) $ molecularMass chem |/| avogadroConstant |*| qSq dopplerDiff 
          |/| (2 *| kB |*| env's temperature |+| 
               molecularMass chem |/| avogadroConstant |*| qSq (fieldToVelocity env chem))
        
        dopplerDiff :: Velocity
        dopplerDiff = dv |-| 
                      (cos phi * sin incli) *| (env's orbitalVelocity)
        
        peakRadiance :: QofU SpectralRadiance 
        peakRadiance = 
          lineRadiation 
            (env's gasSurfaceDensity |/| protonMass |*| fractionalAbundance100au chem) 
            (fieldToVelocity env chem) j (env's temperature) chem
           