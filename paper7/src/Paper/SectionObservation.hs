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
  
  [rawQ| It is possible to observe (+1)-charged chemical species by radio telescopes such as ALMA. |]
  raw "Observations of $\\mathrm{HCO}^{+}$,  $\\mathrm{DCO}^{+}$ and  $\\mathrm{N_2H}^{+}$    lines have been performed "
  citep ["bibcode:2011ApJ...734...98O", "bibcode:2010ApJ...720..480O"]
  raw ". "
  raw "We can distinguish lightning model by observing such charged molecules."
  raw "\n\n"  
  [rawQ|  

The electric field of LMG accelerates the charged chemical species.   
Let  $\varepsilon_I$ be the kinetic energy of a particle of such an ion species $I$.
At the equilibrium $\varepsilon_I = e E_{\rm crit} l_{{\rm mfp},I}$.
The dielectric strength $E_{\rm crit}$ is proportional to the gas number density $n_n$.
Let $A$ be the proportionality factor and $E_{\rm crit} = A n_n$ .
Now, the mean free path $l_{{\rm mfp},I} = 1/\sigma_I(\varepsilon_I) {n_n}$ is inversely proportional to the gas number density $n_n$.
This means that the obtained kinetic energy $\varepsilon_I$ is independent of the gas number density.

\begin{eqnarray*}
 \varepsilon_I &=& e E_{\rm crit} l_{{\rm mfp},I} \\
 &=&e \frac {A n_n} {\sigma_I(\varepsilon_I) n_n} \\
 &=& \frac {e A } {\sigma_I(\varepsilon_I) } 
\end{eqnarray*}

 
The value of $\sigma_I(\varepsilon_I)$
 only depends on the lightning model, so is is universally the same  in a protoplanetary disk.
This gives rise to the unique features of the lightning models in the disk.

The predicted 
$\varepsilon_I$
and the velocities of the ion species are as follows (units are in cm/s);

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
    in order to study  the  ability to distinguish the lightning model from the line observations.
    We assume that our model disk is located at the same position as
    TW Hya i.e. at the distance of 56pc and the
    inclination angle of $7^\circ$ @{citep ["doi:10.1086/421063"]} .

    In Figure \ref{fig-lightning-lp}, 
    simulated integrated emission maps of the $\mathrm{HCO}^{+}$ line for
    N, R25, and R50 disk models are shown.
    

   \begin{figure}
   \begin{tabular}{ccccccc}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-N-24.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-N-32.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-N-36.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-N-40.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-N-44.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-N-48.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-N-56.eps}  \\
   \includegraphics[angle=270,width=4.5cm]{figure/2d-JRB-24.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-JRB-32.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-JRB-36.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-JRB-40.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-JRB-44.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-JRB-48.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2d-JRB-56.eps}  \\
   \includegraphics[angle=270,width=4.5cm]{figure/2dfar-JRB-24.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2dfar-JRB-32.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2dfar-JRB-36.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2dfar-JRB-40.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2dfar-JRB-44.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2dfar-JRB-48.eps}  \hspace{ -3.1cm}& \hspace{ -3.1cm}
   \includegraphics[angle=270,width=4.5cm]{figure/2dfar-JRB-56.eps}  
   \end{tabular}
   \caption{
   Simulated Integrated Emission Map of  $\mathrm{HCO}^{+}$ lines for
   disk models N (upper row),
   R25 (middle row), and
   R50  (lower row), respectively.
   Units are in $\mathrm{Jy} ~ \mathrm{beam}^{ -1} ~ \mathrm{km} ~\mathrm{s}^{ -1}$.
   We assume beam size $0''.65 \times 0''44$.
   \label{figEmissionMap}
   }
   \end{figure}
   
   |]


