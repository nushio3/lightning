{-# LANGUAGE OverloadedStrings #-}
module Paper.SectionModel where

import           Control.Monad.Author
import           Control.Monad.RWS
import qualified Text.LaTeX as LTX
import qualified Text.LaTeX.Utils as LTX

import           Model.Disk.Hayashi (hayashiModelDoc)

sectionModel :: Monad m => AuthorT m ()
sectionModel = do
  tell $ LTX.section "Model"
  tell $ LTX.citet "hayashi_structure_1981"
  hayashiModelDoc
