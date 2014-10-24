{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Paper.SectionMatchedFilter (sectionMF) where

import           Control.Lens(_1, (%~), (&), view)
import           Control.Monad.State
import qualified Text.LaTeX as LTX
import           Text.LaTeX.Base.Class (LaTeXC(..))

import Data.Metrology.Poly
import Data.Metrology.SI.Poly
import Data.Metrology.Synonyms

import Model.Gas
import Model.Observation
import Model.Breakdown

import           Text.Authoring
import           Text.Authoring.TH


sectionMF :: MonadAuthoring s w m => m ()
sectionMF = do
  introMF 
  compareTables
  closingMF


introMF :: MonadAuthoring s w m => m ()
introMF = do
  [rawQ|
   We apply the matched filtering method @{citep ["doi:10.1109/PROC.1963.2383"]},
   in order to distinguish lightning model by ALMA.
   Matched filtering is
   the optimal method for discriminating models under noisy observation has been well studied
   and have wide  applications not only in radio astronomy 
   @{citep [ "bibcode:2003ApJS..147..167E"]} but also in
   extra solar planet astronomy @{citep ["bibcode:1996Icar..119..244J", "bibcode:2000ApJ...535..338D"]}, 
   gravitational wave astronomy @{citep ["bibcode:1999PhRvD..60b2002O","bibcode:2007PhRvD..76h4020V","bibcode:2013PhRvD..88d4026H"]},
   and even in ocean tomography @{citep [ "bibcode:1979DSRA...26..123M"]}.
   We follow the treatment by @{citet ["isbn:978-3-527-40886-3"]} .

   Given that the noise levels for $\mathrm{HCO}^{+}$,  $\mathrm{DCO}^{+}$ and  $\mathrm{N_2H}^{+}$ are
   $#{ppEIn 3 (noiseLevelPerbeam HCOPlus) (Jansky) }$,
   $#{ppEIn 3 (noiseLevelPerbeam DCOPlus) (Jansky) }$, and
   $#{ppEIn 3 (noiseLevelPerbeam N2HPlus) (Jansky) }$, respectively,
   their noise spectrum power density $S_h$ per square arcsecond are
   $#{ppEIn 3 (psdPerAS2 HCOPlus) psdUnit }$,
   $#{ppEIn 3 (psdPerAS2 DCOPlus) psdUnit }$, and
   $#{ppEIn 3 (psdPerAS2 N2HPlus) psdUnit }$, respectively.

   The measure-of-sensitivity $\sigma_{\rm mos}$ of the matched-filter
   between two images $h_1(x,y,v)$ and $h_2(x,y,v)$ is:
   \begin{eqnarray}
      \sigma_{\rm mos} = 4\int_{v_{\rm min}}^{v_{\rm max}} \int_{(x,y)\in {\rm image}}
      \frac{|h_1(x,y,v) - h_2(x,y,v)|^2}{S_h} \mathit{dx}  \mathit{dy} \mathit{dv} .
   \end{eqnarray}
   Here, $x$ and $y$ are image coordinates in arcseconds, and $v$ is the velocity coordinate.
  |]

  where 
    psdUnit =  Jansky :^ sTwo :* (kilo Meter :/ Second)


compareTables :: MonadAuthoring s w m => m ()
compareTables = 
  [rawQ|

  The measure of sensitivity among {\tt N}, {\tt T25}, {\tt DP25} and {\tt R25} models,
  using the $\mathrm{HCO}^{+}$  line, is as follows:

  @{compareTable 25 [HCOPlus]}
 
  The measure of sensitivity among {\tt N}, {\tt T50}, {\tt DP50} and {\tt R50} models are as follows:
  
  @{compareTable 50 [HCOPlus]}


  $\mathrm{DCO}^{+}$ and $\mathrm{N_2H}^{+}$ lines.

  @{compareTable 25 [DCOPlus]}   @{compareTable 50 [DCOPlus]}

  @{compareTable 25 [N2HPlus]}   @{compareTable 50 [N2HPlus]}

  Finally, using all the three lines, we can achieve the following measure-of-sensitivities:

  @{compareTable 25 [HCOPlus, DCOPlus, N2HPlus]}   @{compareTable 50 [HCOPlus, DCOPlus, N2HPlus]}

  |]

compareTable :: MonadAuthoring s w m => Int -> [ChemicalSpecies] ->  m ()
compareTable n chems = 
  [rawQ| 
  
  \begin{tabular}{|c|ccc|}
  \hline
  #{chemStr} &  T#{n} & DP#{n} & R#{n} \\
  \hline
  N      &  #{pp no jtb} &  #{pp no jdpb} &  #{pp no jrb} \\
  T#{n}  &               & #{pp jtb jdpb} &  #{pp jtb jrb} \\
  DP#{n} &               &                &  #{pp jdpb jrb} \\
  \hline
  \end{tabular}

   |]

  where
    pp a b = ppValF "%.1f" $ qSum [measureOfSensitivity n chem  a b| chem <- chems]
    no = Nothing
    jrb = Just RunawayBreakdown
    jdpb = Just DPBreakdown
    jtb = Just TownsendBreakdown

    chemStr :: String
    chemStr
      | chems == [HCOPlus] = "$\\mathrm{HCO}^{+}$" 
      | chems == [DCOPlus] = "$\\mathrm{DCO}^{+}$" 
      | chems == [N2HPlus] = "$\\mathrm{N_2H}^{+}$"
      | otherwise         = show (length chems) ++ " species"


upperLimitTable :: MonadAuthoring s w m => Int -> [ChemicalSpecies] ->  m ()
upperLimitTable n chems = 
  [rawQ| 
  
  \begin{tabular}{|c|ccc|}
  \hline
  #{chemStr} &  T#{n} & DP#{n} & R#{n} \\
  \hline
  N      &  #{pp no jtb} &  #{pp no jdpb} &  #{pp no jrb} \\
  T#{n}  &               & #{pp jtb jdpb} &  #{pp jtb jrb} \\
  DP#{n} &               &                &  #{pp jdpb jrb} \\
  \hline
  \end{tabular}

   |]

  where
    pp a b = ppFIn "%.1f" (critRad a b) AU
    critRad a b = qSqrt $ 
       thresholdSigma *| (r2 |^ sTwo |-| r1 |^ sTwo )
       |/| qSum [measureOfSensitivity n chem  a b| chem <- chems]

    r1 = (fromIntegral n) % AU
    r2 = (fromIntegral n * 2) % AU

    no = Nothing
    jrb = Just RunawayBreakdown
    jdpb = Just DPBreakdown
    jtb = Just TownsendBreakdown

    chemStr :: String
    chemStr
      | chems == [HCOPlus] = "$\\mathrm{HCO}^{+}$" 
      | chems == [DCOPlus] = "$\\mathrm{DCO}^{+}$" 
      | chems == [N2HPlus] = "$\\mathrm{N_2H}^{+}$"
      | otherwise         = show (length chems) ++ " species"


thresholdSigma :: Double
thresholdSigma = 5.0



closingMF :: MonadAuthoring s w m => m ()
closingMF = do
  [rawQ|
   The measure-of-sensitivity for any two different models is larger than 100.
   Therefore the image like Figure \ref{figEmissionMap} is not difficult to detect.
   However, no observation of protoplanetary disk has been reported.
   Therefore, we can reject such form of lightning models from observations. 
   There are multiple alternative scenarios that observations suggest:
   (1) Protoplanetary disk lightning does not exist at all. (2) The probability of protoplanetary disk 
   with lightning matrix gas (LMG) is low, so that we have not yet observed one yet.
   (3) Protoplanetary disk LMG
   exists in forms of LMG clumps (protoplanetary ``cumulonibus clouds'') much smaller than the size of
   the protoplanetary disks.
   
   We can put the upper limit to the size of such LMG clumps by thresholding the measure-of-sensitivity.
   For example, if the radii of LMG clumps is smaller than the values in the following tables,
   they are not $#{thresholdSigma}-\sigma$ detectable.

   @{upperLimitTable 25 [HCOPlus, DCOPlus, N2HPlus]}
   @{upperLimitTable 50 [HCOPlus, DCOPlus, N2HPlus]}

   The above values are upper limit to the size of the LMG clums that possibly exists
   on 
   $25{\mathrm{au}} < r < 50{\mathrm{au}}$ and
   $50{\mathrm{au}} < r < 100{\mathrm{au}}$ orbit, respectively.    
  |]

