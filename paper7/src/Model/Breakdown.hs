{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Breakdown where


import Text.Authoring
import Text.Authoring.TH
import Data.Metrology
import Data.Metrology.SI
import Data.Metrology.Synonyms
import Data.Metrology.Show
import Model.Values


import Model.Gas

data BreakdownModel 
  = TownsendBreakdown
  | DPBreakdown
  | RunawayBreakdown
  deriving (Eq, Show)

breakdownModels = [TownsendBreakdown, DPBreakdown, RunawayBreakdown]

[declareLabels| ntpAirDielectricStrength, takahashiDischargeFormula |]
  

bigMOfAir :: QofU Gram 
bigMOfAir = airMix molecularMass |/| avogadroConstant



aboutDielectricStrengthOfAir :: MonadAuthoring s w m => m ()    
aboutDielectricStrengthOfAir = do  
  
  let 
      citeDSofAir   = citep ["isbn:9780028645865"]    
      french1996    = citep ["doi:10.1029/96JD01625"]      
      dye1986       = citep ["doi:10.1029/JD091iD01p01231"]
      takahashi1983 = citep ["bibcode:1983JMSJ...61...656"]
      takahashi1999 = citep ["bibcode:1999JAtS...56.1561T"]
  
  
  [rawQ|

Dielectric strength of an insulating material is the maximum amplitude 
of the electric field the subject material does not cause the electric
breakdown. It is physical property of central importance for discharge 
physics.
Lightning on Earth is discharge phenomenon in the air, but it has been a long standing 
mystery that lightning takes place under electric field amplitude well below the 
dielectric strength of air. 

On the one hand, the dielectric strength of air at normal temperature and pressure 
(NTP; $20^{\circ}{\rm C}$ and 1atm)
is well established from laboratory experiments @{citeDSofAir}: |]

  environment "eqnarray" $ [rawQ| E_0 = 30 {\rm kV/cm}. @{label ntpAirDielectricStrength}|]


  [rawQ| The long-distance limit of Paschen's law states that the dielectric strength of gas 
depends linearly on the gas number density @{citep ["isbn:978-3-642-64760-4"]}.
However, in the case of the air the dependence of the dielectric strength on the number density is
known to be steeper than linear. This is explained by the electron loss via three-body interactions,
and empirical formulae are known
 @{citep ["doi:10.1063/1.323084","isbn:9784130627184"]}:
         |] 
  environment "eqnarray" $ do
     [rawQ| E &=& E_0 \left( \frac{P}{P_0} \right) ^ {1.5 - 1.65} @{label takahashiDischargeFormula} , |]
  [rawQ| where $E_0$ and $P_0$ are the dielectric strength and the pressure of the air at ground level, respectively. |]



  [rawQ| 

On the other hand, intracloud lightning
is observed with electric field amplitude of
$140 {\rm V/cm}$ @{french1996} to
$150 {\rm V/cm}$ @{dye1986}.
Cloud-to-ground lightning is observed with electric field amplitude of around
$1 {\rm kV/cm}$ @{takahashi1983} to
$2 {\rm kV/cm}$ @{takahashi1999} .
  |]  


{-
 \\\ \\\
  \\\ \\\ \\\\\\\\
   \\\ \\\ \\\\\\\
   /// /\\\       
  /// ///\\\ \\\\\
 /// ///  \\\ \\\\
/// ///    \\\
-}


aboutThreeDischargeModel :: MonadAuthoring s w m => m ()  
aboutThreeDischargeModel = do


  [escQ| We compare following three models of breakdown model: |]
  
  environment "itemize" $ do

    raw "\\item[{\\tt [T]} ]"
    
    [rawQ| 
In {\em Townsend breakdown model} the critical electric field
is such that an electron accelerated by the elecric field 
over its mean free path gains
kinetic energy large enough to ionize a neutral gas molecule.
It has widely been used in meteorological context,
and also adopted into astrophysical context e.g. by
@{citet ["doi:10.1006/icar.1999.6245", "bibcode:2010MNRAS.401.2641M"]} .
This model explains laboratory gas discharge experiments 
(Equations (@{ref ntpAirDielectricStrength})) well.


 |]

    raw "\\item[{\\tt [DP]} ]"

    let dandp = citet ["doi:10.1103/RevModPhys.12.87"]

    [rawQ| 
@{dandp} has derived the formulae for equilibrium distribution
of electron under constant electric field, neglecting the effects of inelastic 
collision with atoms.
{\em Druyversteyn-Penning breakdown model} states that the breakdown 
takes place when the mean electron kinetic energy in Druyversteyn-Penning 
distribution exceeds the ionization energy.
The model is introduced as a protoplanetary disk lightning model by 
@{citet ["doi:10.1086/432796"]}.    |]

    raw "\\item[{\\tt [R]} ]"

    
    [rawQ|
@{citet ["doi:10.1016/0375-9601(92)90348-P"] -- Gurevich+ } 
have propesed {\em Runaway breakdown model} 
and 
@{citet ["doi:10.1070/PU2001v044n11ABEH000939"] -- Gurevich & Zybin}
provided a detailed review of the model.
This model states that breakdown is caused by exponential 
increase of the number of electron with relativistic ($\sim 1$MeV) kinetic energy.
Because the mean free path for such fast electrons is much longer than that for thermal
electrons, runaway breakdown can take place at electric field much weaker than that of a Townsend breakdown. 
Runaway breakdown model better explains the lightning observations in Earth atmosphere and is used as the discharge
model in thunderstorm simulations studies
e.g. by @{citet ["bibcode:2002JGRD..107.4075M"] -- Mansell et al}. |]
    
  
  let landoltBoernstein
       = citep [ "isbn:3-540-64296-X" -- atoms
               , "isbn:3540653473" -- atomic ions
               , "isbn:354044338X" -- molecules
               ]

  [escQ|
In order to calculate the dielectric strength of gas
we need to compute Boltzmann distribution of electrons.
Since the interactons of electrons with even the simplest atoms and molecules
has profound details @{landoltBoernstein},
this requires difficult numerical computations @{citep ["doi:10.1063/1.329081"]}.
In this paper, we will instead resort to a simple calculation that reproduces
the values from the Townsend breakdown model.
 |]





aboutAir :: MonadAuthoring s w m => m ()
aboutAir = do
  [rawQ|
We assume that air consists of
78\% $\rm N_2$,  21\% $\rm O_2$, and 1\% $\rm Ar$ (volume fraction).
Air number density at NTP is $#{ppValE 3 airNumberDensity} {\rm cm^{ -3}}$ .
The ionization energy of these chemical species are
$\Delta W_{\rm N_2} = #{ppFIn "%.1f" (ionizationEnergy N2) ElectronVolt} $,
$\Delta W_{\rm O_2} = #{ppFIn "%.1f" (ionizationEnergy O2) ElectronVolt}$, and
$\Delta W_{\rm Ar}  = #{ppFIn "%.1f" (ionizationEnergy Ar) ElectronVolt}$, respectively.
Of these $\Delta W_{\rm O_2} \sim 12 {\rm eV}$ is the smallest, so we estimate
the electric field amplitude $E_{\rm crit}$ required to accelerate the electron
upto 12eV; i.e. we solve $12 {\rm eV} = e E_{\rm crit} l_{\rm mfp}$.
Given that
the inelastic cross sections of $\rm N_2, O_2, Ar$ for 12 eV electron 12eV are
$0.8, ~1.8, ~0.0 \times 10^{ -16} {\rm cm^{ -2}}$
@{citep ["isbn:3-540-64296-X", "isbn:354044338X"]},
the mean inelastic cross section of air at 12eV is 
$#{ppValE 1 $ airMix $ inelCrossSection 12} {\rm cm^{ -2}}$.
Therefore, $l_{\rm mfp} = (n_n \sigma_{inel})^{ -1 } = #{ppValE 1 mfpAir12} {\rm cm}$.
This gives 
$E_{\rm crit} = #{ppValF "%.0f" $ 1e-3 *| airDielectricStrengthT} {\rm kV/cm}$,
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
since mean molecular weight of air is #{ppFIn "%0.2f" (airMix molecularMass) (Gram :/ Mole)},
 $M = #{ppEIn 2  bigMOfAir Gram}$. |] 


  [rawQ|
Note that $l_{\rm mfp}$ in Druyversteyn-Penning model means elastic
mean free path
$l_{\rm mfp} = (n_n \sigma_{el})^{ -1} = #{ppValE 2 mfpAir12E} {\rm cm}$.
$l_{\rm mfp}$ is calculated from elastic cross sections of the elemental molecules at 12eV
($\sigma_{el} = 
 #{ppEIn 2 (elCrossSection 12 N2) (centi Meter :^ pTwo)}, 
 #{ppEIn 2 (elCrossSection 12 O2) (centi Meter :^ pTwo)}, 
 #{ppEIn 2 (elCrossSection 12 Ar) (centi Meter :^ pTwo)}$,
respectively, for $\rm N_2, O_2, Ar$, see @{citet ["isbn:3-540-64296-X", "isbn:354044338X"]}.)
Therefore,
$E_{\rm crit} = #{ppFIn "%.2f" airDielectricStrengthDP (kilo Volt :/ centi Meter)} $.

Finally, according to the runaway breakdown model the dielectric strength
$E_{\rm crit}$ is the electric field amplitude where
the acceleration by the electric field balances
the ionization loss for minimum ionizing electrons. 
Minimum ionizing electrons are electrons with such kinetic energy $\varepsilon$
that  for them the ionization loss is the smallest.
The ionization loss of an electron as a function of $\varepsilon$ 
is formalized by 
@{citet["doi:10.1002/andp.19303970303", "doi:10.1007/BF01342532","doi:10.1007/BF01344553"]}.
We use the following form of Bethe formula from
@{citationGen "citet[chap 5.5]" ["isbn:978-0-521-75618-1"]}:
   |]

  environment "eqnarray" $ do
    [rawQ| -\frac{d\varepsilon}{dt} &=& 
\frac{e^4 {\bar Z} n_n}{8 \pi {\epsilon_0}^2 m_e c^2}a(\gamma), \\
\mathrm{where}~~~a(\gamma) &=& \left(\frac{c}{v}\right)^2
   \left[ \ln\frac{\gamma^3 {m_e}^2 v^4 }{2 (1+\gamma){\bar I}^2}
 - \left(\frac{2}{\gamma} - \frac{1}{\gamma^2}\right)\ln 2
 + \frac{1}{\gamma^2}
 + \frac{1}{8}\left(1 - \frac{1}{\gamma}\right)^2\right].
    |]

  [rawQ|
Here,
$ \gamma = (1 - v^2/c^2)^{ -1/2} $ is the Lorentz factor of the electron,
$\varepsilon = (\gamma - 1) m_e c^2$ is the electron kinetic energy, 
${\bar Z} n_n$ is the number density of ambient electrons of the matter. 
$\bar I$ is the mean excitation energy, a parameter to be fitted to
laboratory experimental data. We use the value of 
$\bar I_{\rm air} = 86.3 {\rm eV}$ from ESTAR database @{citep ["special:nist-estar"]}.

For the case of the air $a(\gamma)$ takes its minimum 
$a_{\rm min} = 20.2$ at $\gamma = 3.89$
or $\varepsilon = 1.48 {\rm MeV}$. The dielectric strength $E_{\rm crit}$
is the solution of the following work-balance equation
\begin{eqnarray}
  eE-\frac{d\varepsilon}{dt} &=& 0,
\end{eqnarray}
which is 
\begin{eqnarray}
E_{\rm crit} 
&=& \frac{e^3 {\bar Z} n_n}{8 \pi {\epsilon_0}^2 m_e c^2}a_{\rm min}, \nonumber \\
&=&  #{ppFIn "%.1f" airDielectricStrengthR (kilo Volt :/ centi Meter)} .
\end{eqnarray}
   |]



  [rawQ|
All the three model state that the dielectric strength of the gas is proportional to
the number density of the gas. 

\begin{eqnarray}
\begin{array}{CCCCC}
E_{\rm c, T} &=& \frac{\Delta W}{e} (\sigma_{\mathrm tot} - \sigma_{\mathrm el})  n_n&=&
 #{ppFIn "%.1f"  airDielectricStrengthT  (kilo Volt :/ centi Meter)} \cdot 
 \left( \frac{n_n}{n_{0,\mathrm{air}}}  \right)^{1}   , \\
E_{\rm c,DP} &=& \frac{\Delta W}{0.43} \sqrt{\frac{m_e}{M}} \sigma_{\mathrm el} n_n  &=&
 #{ppFIn "%.1f"  airDielectricStrengthDP (kilo Volt :/ centi Meter)} \cdot
 \left( \frac{n_n}{n_{0,\mathrm{air}}}  \right)^{1}  , \\
E_{\rm c, R} &=& \frac{e^3 a_{\rm min} {\bar Z} }{8 \pi \epsilon_0 m c^2} n_n&=&
 #{ppFIn "%.1f"  airDielectricStrengthR  (kilo Volt :/ centi Meter)} \cdot
 \left( \frac{n_n}{n_{0,\mathrm{air}}}  \right)^{1}   .
\end{array}
\end{eqnarray}
  |]

--  (log((511e3/88.0)**2/2 * x**3/(1+x)*(1-1/x**2)**2)- (2/x - 1/x**2)* log (2) + 1/x**2 + (1-1/x)**2/8)/(1-1/x**2)

-- This is the database to use
-- http://physics.nist.gov/PhysRefData/Star/Text/ESTAR.html

-- other databases
-- http://www.srim.org/SRIM/SRIMPICS/IONIZ.htm
-- http://physics.nist.gov/PhysRefData/XrayMassCoef/tab1.html


