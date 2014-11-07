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

import Data.Metrology.Poly
import Data.Metrology.Synonyms

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

  command1 "subsection" $ raw "Estimation of the line signal strength"
  
  [rawQ| 
  The goal of this section is to calculate the Doppler broadening of the molecular ion lines in the disk,
  which reflects the electric field strength in the protoplanetary disk.
  In order to establish the observation procedure, we calculate the collisional cross sections and the terminal
  velocities of the molecules. Then, we can estimate the optical depths and the spectral irradiances of the specific
  lines. We simulate the observational images using the calculated spectral irradiances. Finally, we establish a model discrimination procedure based on matched-filtering.


  We choose three ion species: $\mathrm{HCO}^{+}$,  $\mathrm{DCO}^{+}$ and  $\mathrm{N_2H}^{+}$  lines, whose observations have been performed 
  @{citep ["bibcode:2011ApJ...734...98O", "bibcode:2010ApJ...720..480O"]} .
Such charged chemical species are accelerated upto their respective terminal velocity by
the electric field of the LMG.
Let  $\varepsilon_I$ be the kinetic energy of a particle of such an ion species $I$.
At the equilibrium $\varepsilon_I = e E_{\rm crit} l_{{\rm mfp},I}$.
As shown in equations (\ref{eq:DischargeAir}) and (\ref{eq:DischargeDisk}), 
the dielectric strength $E_{\rm crit}$ is proportional to the gas number density $n_n$.
Let $A$ be the proportionality factor and $E_{\rm crit} = A n_n$ .
Now, the mean free path $l_{{\rm mfp},I} = 1/\sigma_I(\varepsilon_I) {n_n}$ is inversely proportional to the gas number density $n_n$.
This means that the obtained kinetic energy $\varepsilon_I$ is independent of the gas number density.

\begin{eqnarray*}
 \varepsilon_I &=& e E_{\rm crit} l_{{\rm mfp},I} \\
 &=& \frac {e A } {\sigma_I(\varepsilon_I) } 
\end{eqnarray*}

 
The value of $\sigma_I(\varepsilon_I)$
 only depends on the lightning model, so is is universally the same  in a protoplanetary disk.
This gives rise to the unique features of the lightning models in the disk.
The predicted 
$\varepsilon_I$
and the velocities of the ion species
(Table \ref{tbl:terminalVelocity}).



\begin{table}
\begin{center}
  \begin{tabular} {|c|ccc|}
\hline
 &  $\mathrm{HCO}^{+}$ & $\mathrm{DCO}^{+}$ & $\mathrm{N_2H}^{+}$ \\
\hline
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
\hline
\end{tabular}
\end{center}
\caption{The terminal velocity of the molecular ions which we use as the characteristic features
for observational measurement of the dielectric strength.
Units are in cm/s.
}\label{tbl:terminalVelocity}
\end{table}


In the Appendix we describe
the detail
of the cross section model we have used in order to estimate the above cross sections.

  |]

  aboutLineObservation

  subsectionFigures

  return ()



subsectionFigures :: MonadAuthoring s w m => m ()
subsectionFigures = do
  [rawQ|
   \subsection{Line Profile Prediction by Radiative Transfer}

   We introduce the following seven disk models.

   \begin{tabular}{|c|c|c|}   
     \hline                             
     disk model name & discharge & LMG region \\
     \hline                                   
     N   & no discharge  & \\
     T25 & Townsend discharge & $25{\mathrm{au}} < r < 50{\mathrm{au}}$\\
     T50 & Townsend discharge& $50{\mathrm{au}} < r < 100{\mathrm{au}}$\\     
     DB25 & Druyversteyn-Penning discharge & $25{\mathrm{au}} < r < 50{\mathrm{au}}$\\ 
     DB50& Druyversteyn-Penning discharge & $50{\mathrm{au}} < r < 100{\mathrm{au}}$\\
     R25 & runaway dischage & $25{\mathrm{au}} < r < 50{\mathrm{au}}$\\     
     R50& runaway dischage & $50{\mathrm{au}} < r < 100{\mathrm{au}}$\\    
     \hline
   \end{tabular}
   
    
   \begin{figure}
   \includegraphics[angle=270,width=8cm]{figure/mix-HCOPlus-V50x80-pv.eps}
   \includegraphics[angle=270,width=8cm]{figure/mix-DCOPlus-V50x80-pv.eps}
   \includegraphics[angle=270,width=8cm]{figure/mix-N2HPlus-V50x80-pv.eps}
   \caption{
    The line profiles for  $\mathrm{HCO}^{+}$ , $\mathrm{DCO}^{+}$ and $\mathrm{N_2H}^{+}$,
      assuming that the lightning takes place at $ \mathrm{50au} < r <  \mathrm{100au}$ of the disk.
    The labels {\tt no}, {\tt T}, {\tt DP}, and {\tt R} for the curves corresponds to 
    no lightning, Townsend breakdown model, Druyversteyn-Penning breakdown model and
    runaway breakdown model, respectively.
   }\label{fig-lightning-lp}
   \end{figure}    
    
    We calculate the line profiles for the three ion species with these seven disk models,
    in order to study the ability to distinguish the lightning model from the line observations (Figure \ref{fig-lightning-lp}).
    The line profiles are obtained by performing the spectral irradiance integral (equation ({eq:SpectralIrradiance})).
    In Figure \ref{figEmissionMap}, we present the
    simulated integrated emission maps of the $\mathrm{HCO}^{+}$ line for
    N, T25, and T50 disk.
    We assumed that our model disk is located at the same position as
    TW Hya i.e. at the distance of 56pc and the
    inclination angle of $7^\circ$ @{citep ["doi:10.1086/421063"]}.
    Our programs can be easily applied to other disk systems.

    

   \begin{figure}
   \begin{tabular}{ccccccc}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-N-24.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-N-32.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-N-36.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-N-40.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-N-44.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-N-48.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-N-56.eps}  \\
   \includegraphics[angle=270,width=4.5cm]{figure/2d-JTB-24.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-JTB-32.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-JTB-36.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-JTB-40.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-JTB-44.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-JTB-48.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-JTB-56.eps}  \\
   \includegraphics[angle=270,width=4.5cm]{figure/2dfar-JTB-24.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2dfar-JTB-32.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2dfar-JTB-36.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2dfar-JTB-40.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2dfar-JTB-44.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2dfar-JTB-48.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2dfar-JTB-56.eps}  
   \end{tabular}
   \caption{
   Simulated Integrated Emission Map of  $\mathrm{HCO}^{+}$ lines for
   disk models N (upper row),
   T25 (middle row), and
   T50  (lower row), respectively.
   Units are in $\mathrm{Jy} ~ \mathrm{beam}^{ -1} ~ \mathrm{km} ~\mathrm{s}^{ -1}$.
   We assume beam size $0''.65 \times 0''44$.
   \label{figEmissionMap}
   }
   \end{figure}
   
   |]


