{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Paper.SectionCrossSection where

import           Control.Monad.RWS
import           Text.Authoring
import           Text.Authoring.TH

sectionCrossSection :: MonadAuthoring s w m => m ()
sectionCrossSection = do
  command1 "section" $ raw "Appendix: Cross Section Model of Ion-Neutral Molecular Collision"
  [rawQ| 
     We establish the model of ion-neutral collisional cross sections as functions of collision energy,
in collaboration with Motomichi Tashiro.

In order to compute the equilibrium speed under electric field for ion species
 $\rm HCO^{+}$ , $\rm DCO^{+}$  and $\rm N_2H^{+}$, we need the knowledge of
the energy-dependent cross section of the collisions between the 
 $\rm H_2$ and the ion species. 
However, no experimental values for such collisions exist.
In general, collisional cross section data for molecular ions and molecules are scarce, due to 
the difficulty of setting up such collision experiments.
On the other hand, quantum-mechanical simulation of such collision event 
would require upto 1 month per single collision
(Tashiro, private comm.,) and it requires many collision simulations with different 
collision parameters to establish a cross section value for one collision energy.
The computational cost prohibits the simulational estimation of the collisional cross section.

Therefore, we construct and use a simple empirical model of 
 molecular ions and molecules collisional cross section, following
Tashiro's advice.

There are  collision cross section data 
  @{citep ["bibcode:1990NISTJ..95..407P", "bibcode:1991JPCRD..20..557P"]}.
for the following six pairs of  molecular ions and molecules:
${\rm H^{+}}-{\rm H_2}$ ,
${\rm {H_2}^{+}}-{\rm H_2}$ ,
${\rm {H_3}^{+}}-{\rm H_2}$ ,
${\rm {N}^{+}}-{\rm N_2}$ ,
${\rm {N_2}^{+}}-{\rm N_2}$ , and
${\rm {Ar}^{+}}-{\rm Ar}$ .
We use the following model of total collisional cross section
$\sigma_{I^{+}, I}(\varepsilon)$ between molecular ion species
$I^{+}$ and ion species $I$:
\begin{eqnarray}
\sigma_{I^{+}, I}(\varepsilon) = A(\varepsilon) \mu(I^{+}, I)^{p(\varepsilon)},
\label{eq-fit-model}
\end{eqnarray}
where 
$\varepsilon$ is the collision energy,
  $\mu(I^{+}, I)$ is the residual mass of species
$I^{+}$ and $I$.
 $A(\varepsilon)$ and 
$p(\varepsilon)$ are model parameters.
 
We perform the fitting of the model so that the following cost function $C$

\begin{eqnarray}
C = \sum_{\varepsilon,I^{+}, I } 
\left(\sigma_{I^{+}, I}(\varepsilon) - \sigma_{I^{+}, I,\rm exp}[\varepsilon]
\right)^2
\end{eqnarray}

is minimized. Here, $\sigma_{I^{+}, I,\rm exp}[\varepsilon]$ 
are experimentally known cross section values for some fixed values of $\varepsilon$
found in
  @{citet ["bibcode:1990NISTJ..95..407P", "bibcode:1991JPCRD..20..557P"]}. 
The experimental data and the best-fit models are presented in
Figure \ref{fig-cross-section-model}.

  \begin{figure}
   \begin{center}
\includegraphics[angle=270,width=12cm]{figure/cross-section-model.eps}
   \end{center}
\caption{Cross section model. Experimental data are dots, and fitted models are curves.}
 \label{fig-cross-section-model}
  \end{figure}
   
We have also studied models more complex than equation (\ref{eq-fit-model}).
These complex models reproduce more detailed behavior of the cross sections such as
dependence on ion-neutral molecular mass ratio.
However, there are numerous candidates for such models, and the
predicted cross section values vary upto four degrees of magnitude among the models.
Available experimental data are not abundant enough to justify a choice 
among those complex models without risking overfitting.
Therefore, we decided to use the simple model of equation (\ref{eq-fit-model})
that captures overall tendency.
      
   
        |]



                         