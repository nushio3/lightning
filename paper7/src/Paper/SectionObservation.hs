{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Paper.SectionObservation where

import           Control.Monad.State
import qualified Text.LaTeX as LTX
import           Text.LaTeX.Base.Class (LaTeXC(..))

import           Text.Authoring

sectionObservation :: MonadAuthoring s w m => m ()
sectionObservation = do
  command1 "section" $ raw "Observation"
  raw "Observation of $\\mathrm{HCO}^{+}$ lines are possible"
  citep ["bibcode:2011ApJ...734...98O", "bibcode:2010ApJ...720..480O"]
  raw ". "
  citet ["doi:10.1006/icar.1999.6245"]
  raw " calculates the lightning mean free path. "
  return ()
  