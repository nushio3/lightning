{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Paper.SectionObservation where

import           Control.Monad.Author
import           Control.Monad.Author.Cite(cite)
import           Control.Monad.RWS
import qualified Text.LaTeX as LTX
import qualified Text.LaTeX.Utils as LTX
import           Text.LaTeX.Base.Class (LaTeXC(..))

sectionObservation :: (MonadIO m) => AuthorT m ()
sectionObservation = do
  tell $ LTX.section "Observation."
  () <- cite "bibcode:2011ApJ...734...98O"
  () <- cite "bibcode:2010ApJ...720..480O"
  LTX.raw $ "Observation of $\\mathrm{HCO}^{+}$ lines are possible."
