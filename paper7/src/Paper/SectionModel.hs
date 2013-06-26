{-# LANGUAGE OverloadedStrings #-}
module Paper.SectionModel where

import           Control.Monad.Author
import qualified Text.LaTeX as LTX


sectionModel :: Monad m => AuthorT m ()
sectionModel = do
  LTX.section "Model"
  "distribution was shown."
  2 * 3.14e96

