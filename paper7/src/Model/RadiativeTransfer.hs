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


scovilleFormula :: ChemicalSpecies -> Int -> KelvinUnit Double -> PerCm2 Double
scovilleFormula = undefined



-- http://www.cv.nrao.edu/course/astr534/Equations.html

aboutScoville :: MonadAuthoring s w m => m ()
aboutScoville = do
  [rawQ|
  The column density corresponding to optical depth $\tau_{\nu 0}=1$ is estimated as follows
  @{citationGen "citep[see the appendix of][.]" ["bibcode:1986ApJ...303..416S"]}

  @{aboutScovilleFormula} 

  where $B$ is the rotational constant of the molecule, $\mu$ is its electric dipole matrix element,
  $h \nu_0$ is the energy difference between the two levels 
  and $J$ is the rotational quantum number of the lower state.

  The optical depth of the disk with column density $N$ is

\begin{eqnarray}
\tau_\nu(N) = N / N_{\tau_{\nu 0}=1} ,
\end{eqnarray}


  so that the intensity is

\begin{eqnarray}
I(\nu) = B_{\nu_0}(T) (1-\exp \tau_\nu(N))        .
\end{eqnarray}

 
  Here $ B_{\nu_0}(T) $ is the vacuum brightness of a black body at frequency $\nu_0$.
  We use the Rayleigh-Jeans approximation $B_{\nu_0}(T) = 2k_B T {\nu_0}^2 / c^2$
  (see @{citationGen "citet[chap. 2.7]" ["isbn:3-540-29692-1"]}).

  |]

