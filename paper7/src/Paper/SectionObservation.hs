{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Paper.SectionObservation where

import           Control.Monad.State
import qualified Text.LaTeX as LTX
import           Text.LaTeX.Base.Class (LaTeXC(..))

import UnitTyped
import qualified UnitTyped.NoPrelude as U
import UnitTyped.Synonyms

import Model.Disk
import Model.Disk.Hayashi
import Model.Gas
import Model.Values
import Model.RadiativeTransfer

import           Text.Authoring
import           Text.Authoring.TH



sectionObservation :: MonadAuthoring s w m => m ()
sectionObservation = do
  command1 "section" $ raw "Observation"
  
  [rawQ| It is possible to observe (+1)-charged chemical species by radio telescopes such as ALMA. |]
  raw "Observations of $\\mathrm{HCO}^{+}$  $\\mathrm{DCO}^{+}$ and  $\\mathrm{N_2H}^{+}$ lines have been performed "
  citep ["bibcode:2011ApJ...734...98O", "bibcode:2010ApJ...720..480O"]
  raw ". "
  raw "We can distinguish lightning model by observing such charged molecules. The reason is as follows."
  raw "\n\n"  
  [rawQ|  
Magnetorotational instability (MRI) creates electric field $E$. 
The breakdown model sets upper limit $E \leq E_{\rm crit}$ to the electric field amplitude.
This electric field accelerates the charged chemical species.   
The kinetic energy $\varepsilon$ obtained by this mechanism is $\varepsilon = e E_{\rm crit} l_{\rm mfp}$.
The dielectric strength $E_{\rm crit}$ is proportional to the gas number density $n_n$
while the mean free path $l_{\rm mfp}$ is inversely proportional to the gas number density $n_n$.
This means that the obtained kinetic energy $\varepsilon$ does not depend on the gas number density.
It only depends on the lightning model, so the value is the same anywhere in a protoplanetary disk,
provided that the lightning model does not change within the protoplanetary disk. Each model predicts
the maximum velocities of the chemical elements as follows (units are in cm/s);

  \begin{tabular} {cccc}
 &  $\mathrm{HCO}^{+}$ & $\mathrm{DCO}^{+}$ & $\mathrm{N_2H}^{+}$ \\
T  
  & $#{ppValE 1 $ fieldToVelocity mmsn1au HCOPlus }$
  & $#{ppValE 1 $ fieldToVelocity mmsn1au DCOPlus }$
  & $#{ppValE 1 $ fieldToVelocity mmsn1au N2HPlus }$
  \\
DP
  & $#{ppValE 1 $ fieldToVelocity mmsn1au HCOPlus }$
  & $#{ppValE 1 $ fieldToVelocity mmsn1au DCOPlus }$
  & $#{ppValE 1 $ fieldToVelocity mmsn1au N2HPlus }$
  \\
R 
  & $#{ppValE 1 $ fieldToVelocity mmsn1au HCOPlus }$
  & $#{ppValE 1 $ fieldToVelocity mmsn1au DCOPlus }$
  & $#{ppValE 1 $ fieldToVelocity mmsn1au N2HPlus }$
  \\
\end{tabular}
 
Note that I have used inelastic electron mean free path ($#{ppValE 1 $ mfpPpd15 $ equatorAt1au}$ cm at $r=1$ au.)
We should use chemical-species specific collisional cross section instead. This is a TODO.

We used cross sections for 15eV electrons because $\Delta W_{\rm H_2} = 15.43{\rm eV}$.
The cross sections are as follows @{citep ["isbn:3-540-64296-X", "isbn:354044338X"]}.

  \begin{tabular} {ccc}
species & $\sigma_{\rm inel}$ & $\sigma_{\rm el}$ \\
$\rm H_2$  &  $#{ppValE 1 $ inelCrossSection 15 H2}$ &  $#{ppValE 1 $ elCrossSection 15 H2}$\\
$\rm He$   &  $#{ppValE 1 $ inelCrossSection 15 He}$ &  $#{ppValE 1 $ elCrossSection 15 He}$\\
$\rm CO$   &  $#{ppValE 1 $ inelCrossSection 15 CO}$ &  $#{ppValE 1 $ elCrossSection 15 CO}$\\
$\rm O_2$  &  $#{ppValE 1 $ inelCrossSection 15 O2}$ &  $#{ppValE 1 $ elCrossSection 15 O2}$
\end{tabular}
  |]

  aboutLineObservation

  return ()
