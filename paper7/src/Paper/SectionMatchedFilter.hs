{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Paper.SectionMatchedFilter where

import           Control.Lens(_1, (%~), (&), view)
import           Control.Monad.State
import qualified Text.LaTeX as LTX
import           Text.LaTeX.Base.Class (LaTeXC(..))

import Data.Metrology.Poly
import Data.Metrology.SI.Poly
import Data.Metrology.Synonyms

import Model.Gas
import Model.Observation

import           Text.Authoring
import           Text.Authoring.TH


sectionMF :: MonadAuthoring s w m => m ()
sectionMF = do
  [rawQ|
   The optimal method for discreminating models under noisy observation has been studied.
   We apply the matched filtering method @{citep ["doi:10.1109/PROC.1963.2383"]}
   in order to distinguish lightning model by ALMA.
   We follow the treatment by @{citet ["isbn:978-3-527-40886-3"]} .

   Given that the noise levels for $\mathrm{HCO}^{+}$,  $\mathrm{DCO}^{+}$ and  $\mathrm{N_2H}^{+}$ are
   $#{ppEIn 3 (noiseLevelPerbeam HCOPlus) (Jansky) }$,
   $#{ppEIn 3 (noiseLevelPerbeam DCOPlus) (Jansky) }$, and
   $#{ppEIn 3 (noiseLevelPerbeam N2HPlus) (Jansky) }$, respectively,
   their noise spectrum power density per square arcsecond are
   $#{ppEIn 3 (psdPerAS2 HCOPlus) (Jansky :^sTwo) }$,
   $#{ppEIn 3 (psdPerAS2 DCOPlus) (Jansky :^sTwo) }$, and
   $#{ppEIn 3 (psdPerAS2 N2HPlus) (Jansky :^sTwo) }$, respectively.

  |]

