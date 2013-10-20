{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Breakdown where

import Text.Authoring
import Text.Authoring.TH


[declareLabels| takahashiDischargeFormula |]
  


aboutDielectricStrengthOfAir :: MonadAuthoring s w m => m ()    
aboutDielectricStrengthOfAir = do  
  
  let takahashi2007 = citep ["isbn:9784130627184"] 
  [escQ|

Dielectric strength of an insulating material is the maximum amplitude 
of the electric field the subject material does not cause the electric
breakdown. It is physical property of central importance for discharge 
physics.

Lightning on Earth is discharge phenomenon in the air, but it has been a long standing 
problem that lightning takes place under electric field amplitude well below the 
dielectric strength of air; The dielectric strength of air 
as function of pressure is @{takahashi2007}:
         |]
  
  environment "eqnarray" $ do
    [rawQ| E &=& E_0 \left( \frac{P}{P_0} \right) ^ {1.65} @{label takahashiDischargeFormula} , |]


  let citeDSofAir   = citep [ "doi:10.1063/1.323084", "isbn:9780028645865"]    
      french1996    = citep ["doi:10.1029/96JD01625"]      
      dye1986       = citep ["doi:10.1029/JD091iD01p01231"]
      takahashi1983 = citep ["bibcode:1983JMSJ...61...656"]
      takahashi1999 = citep ["bibcode:1999JAtS...56.1561T"]


  [rawQ| where $E_0 = 30 {\rm kV/cm}$ @{citeDSofAir},
$P_0 = 1 {\rm atm}$ are dielectric strength and pressure 
of air at ground level, respectively. On the other hand, intracloud lightning
is observed with electric field amplitude of
$140 {\rm V/cm}$ @{french1996} to
$150 {\rm V/cm}$ @{dye1986}.
Cloud-to-ground lightning is observed with electric field amplitude of around
$1 {\rm kV/cm}$ @{takahashi1983} to
$2 {\rm kV/cm}$ @{takahashi1999} .
  |]  



aboutThreeDischargeModel :: MonadAuthoring s w m => m ()  
aboutThreeDischargeModel = do


  [escQ| We compare following three models of breakdown model: |]
  
  environment "itemize" $ do

    raw "\\item[{\\tt [C]} ]"
    
    [rawQ| 
In {\em conventional breakdown model} the critical electric field
is such that a thermal electron accelerated by the elecric field 
over its mean free path gains
kinetic energy large enough to ionize a neutral gas molecule.
It has widely been used in meteorological context,
and also adopted into astrophysical context e.g. by
@{citet ["doi:10.1006/icar.1999.6245", "bibcode:2010MNRAS.401.2641M"]} .
This model explains laboratory gas discharge experiments 
(Equations (@{ref takahashiDischargeFormula})) well.


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
{\em Runaway breakdown model}, proposed by 
@{citet ["doi:10.1016/0375-9601(92)90348-P"] -- Gurevich+ } 
and detailed in
@{citet ["doi:10.1070/PU2001v044n11ABEH000939"] -- Gurevich & Zybin}.
In this model ionization that leads to breakdown is caused by exponential 
increase of the number of electron with relativistic ($\sim 1$MeV) kinetic energy.
Because the mean free path for such fast electrons is much longer than that for thermal
electrons, runaway breakdown takes place at electric field much weaker than that of a conventional breakdown. 
Runaway breakdown better explains the lightning observations in Earth atmosphere and is used as the discharge
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
In this paper, we will instead resort to a back-of-the-envelope calculation to reproduce 
the result of the conventional breakdown model.

 |]






