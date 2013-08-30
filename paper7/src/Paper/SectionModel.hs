{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Paper.SectionModel where

import           Control.Monad.Author
import           Control.Monad.Author.Cite(cite)
import           Control.Monad.RWS
import qualified Text.LaTeX as LTX
import qualified Text.LaTeX.Utils as LTX
import           Text.LaTeX.Base.Class (LaTeXC(..))
import           Model.Disk.Hayashi (hayashiModelDoc)

sectionModel :: (MonadIO m) => AuthorT m ()
sectionModel = do
  tell $ LTX.section "Model"
  tell $ LTX.citet "hayashi_structure_1981"
  () <- cite "bibcode:1981PThPS..70...35H"
  hayashiModelDoc
