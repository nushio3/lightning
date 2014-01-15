{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Paper.SectionObservation where

import           Control.Lens(_1, (%~), (&), view)
import           Control.Monad.State
import qualified Text.LaTeX as LTX
import           Text.LaTeX.Base.Class (LaTeXC(..))

import UnitTyped
import qualified UnitTyped.NoPrelude as U
import UnitTyped.Synonyms

import Model.Breakdown
import Model.Disk
import Model.Disk.Hayashi
import Model.Disk.Derived
import Model.Gas
import Model.Values
import Model.RadiativeTransfer

import           Text.Authoring
import           Text.Authoring.TH



sectionObservation :: MonadAuthoring s w m => m ()
sectionObservation = do
  let
      mmsn1auT  = mmsn1au & disk %~ lightenedDisk TownsendBreakdown
      mmsn1auDP = mmsn1au & disk %~ lightenedDisk DPBreakdown
      mmsn1auR  = mmsn1au & disk %~ lightenedDisk RunawayBreakdown
  
  
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
  & $#{ppValE 1 $ fieldToVelocity mmsn1auT HCOPlus }$
  & $#{ppValE 1 $ fieldToVelocity mmsn1auT DCOPlus }$
  & $#{ppValE 1 $ fieldToVelocity mmsn1auT N2HPlus }$
  \\
DP
  & $#{ppValE 1 $ fieldToVelocity mmsn1auDP HCOPlus }$
  & $#{ppValE 1 $ fieldToVelocity mmsn1auDP DCOPlus }$
  & $#{ppValE 1 $ fieldToVelocity mmsn1auDP N2HPlus }$
  \\
R 
  & $#{ppValE 1 $ fieldToVelocity mmsn1auR HCOPlus }$
  & $#{ppValE 1 $ fieldToVelocity mmsn1auR DCOPlus }$
  & $#{ppValE 1 $ fieldToVelocity mmsn1auR N2HPlus }$
  \\
\end{tabular}
 
Note that I have used inelastic electron mean free path ($#{ppValE 1 $ view mfpPpd15 mmsn1au}$ cm at $r=1$ au.)
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

  subsectionFigures

  return ()



subsectionFigures :: MonadAuthoring s w m => m ()
subsectionFigures = do
  [rawQ|
   \subsection{Line Profile Prediction by Radiative Transfer}
   
   \begin{figure}
   \includegraphics[angle=270,width=8cm]{figure/lineProfile-HCOPlus-00.eps}
   \includegraphics[angle=270,width=8cm]{figure/lineProfile-DCOPlus-00.eps}
   \includegraphics[angle=270,width=8cm]{figure/lineProfile-N2HPlus-00.eps}
   \caption{
    The line profiles for  $\mathrm{HCO}^{+}$ , $\mathrm{DCO}^{+}$ and $\mathrm{N_2H}^{+}$,
      assuming that the lightning takes place in the entire disk.
    The labels {\tt no}, {\tt T}, {\tt DP}, and {\tt R} for the curves corresponds to 
    no lightning, Townsend breakdown model, Druyversteyn-Penning breakdown model and
    runaway breakdown model, respectively.
           }\label{fig-lightning-00}

   \end{figure}    
    
   \begin{figure}
   \includegraphics[angle=270,width=8cm]{figure/lineProfile-HCOPlus-ex.eps}
   \includegraphics[angle=270,width=8cm]{figure/lineProfile-DCOPlus-ex.eps}
   \includegraphics[angle=270,width=8cm]{figure/lineProfile-N2HPlus-ex.eps}
   \caption{
    The line profiles for  $\mathrm{HCO}^{+}$ , $\mathrm{DCO}^{+}$ and $\mathrm{N_2H}^{+}$,
      assuming that the lightning takes place at $ \mathrm{2au} < r <  \mathrm{20au}$ of the disk.
    The labels {\tt no}, {\tt T}, {\tt DP}, and {\tt R} are the same meanings as in \ref{fig-lightning-00}.
   }\label{fig-lightning-ex}
   \end{figure}    
    
    We calculated the line profiles for the three ion species in disk with different lightning models,
    to see if we can distinguish the lightning models from the line observations.

    We assume that our model disk is located at the same position as
    TW Hya i.e. at the distance of 56pc and the
    inclination angle of $7^\circ$ @{citep ["doi:10.1086/421063"]} .
    
   
   |]