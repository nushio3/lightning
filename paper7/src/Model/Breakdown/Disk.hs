{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Breakdown.Disk where

import Text.Authoring
import Text.Authoring.TH

import UnitTyped
import qualified UnitTyped.NoPrelude as U
import UnitTyped.Synonyms


import Model.Disk
import Model.Gas
import Model.Values


aboutDiskDischarge :: MonadAuthoring s w m => m ()
aboutDiskDischarge = 
  [rawQ| 
In the MMSN model the gas density at eqatorial plane, $r = 1{\rm au}$ is 
$n_{0,\mathrm{ppd}} = #{ppValE 2 $ ppdDensity equatorAt1au} 
{\rm g~cm^{ -3}}$.
We assume that protoplanetary disk gas consists of 
$\rm H_2,He,CO,O_2$ and their volume fractions are
$0.92, 7.8\times10^{ -2},2.3\times10^{ -4},1.3\times10^{ -4}$, respectively
@{citationGen "citep[chap. 3.4.6.]" ["bibcode:2009LanB...4B...44L"]} 
Calculations similar to the previous section leads to the following values:

\begin{eqnarray}
\begin{array}{CCCCC}
E_{\rm c, T} &=& \frac{\Delta W}{e} (\sigma_{\mathrm tot} - \sigma_{\mathrm el})  n_n&=&
 #{ppValE 1 $ ppdDielectricStrengthT equatorAt1au} \left( \frac{n_n}{n_{0,\mathrm{ppd}}}  \right) \mathrm{V/cm}  , \\
E_{\rm c,DP} &=& \frac{\Delta W}{0.43} \sqrt{\frac{m_e}{M}} \sigma_{\mathrm el} n_n  &=&
 #{ppValE 1 $ ppdDielectricStrengthDP equatorAt1au} \left( \frac{n_n}{n_{0,\mathrm{ppd}}}  \right) \mathrm{V/cm}  , \\
E_{\rm c, R} &=& \frac{e^3 a_{\rm min} {\bar Z} }{8 \pi \epsilon_0 m c^2} n_n&=&
 #{ppValE 1 $ ppdDielectricStrengthR equatorAt1au} \left( \frac{n_n}{n_{0,\mathrm{ppd}}}  \right) \mathrm{V/cm}  .
\end{array}
\end{eqnarray}

|]
