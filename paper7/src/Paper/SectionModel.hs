{-# LANGUAGE OverloadedStrings #-}
module Paper.SectionModel where

import           Control.Monad.Author
import qualified Text.LaTeX as LTX

import           Model.Disk.Hayashi (hayashiModelDoc)

sectionModel :: Monad m => AuthorT m ()
sectionModel = do
  LTX.section "Model"
  "\\citet{hayashi_structure_1981}"
  hayashiModelDoc
  

