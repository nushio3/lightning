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
import Data.Metrology.Synonyms

import           Text.Authoring
import           Text.Authoring.TH


sectionMF :: MonadAuthoring s w m => m ()
sectionMF = do
  [rawQ|
   The optimal method for discreminating models under noisy observation has been studied.
   We apply the matched filtering method @{citep ["doi:10.1109/PROC.1963.2383"]}
   in order to distinguish lightning model by ALMA.
   We follow the treatment by @{citet ["isbn:978-3-527-40886-3"]} .

   Hoge
  |]

