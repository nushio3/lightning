{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Paper.SectionModel where

import           Control.Monad.RWS
import qualified Text.LaTeX as LTX
import qualified Text.LaTeX.Utils as LTX
import           Text.LaTeX.Base.Class (LaTeXC(..))
import           Model.Disk.Hayashi (hayashiModelDoc)
  
import           Text.LaTeX.Author 


sectionModel :: (MonadIO m) => AuthorT m ()
sectionModel = do
  tell $ LTX.section "Model"
  tell $ "We study the following three models of lightning: "
  
  () <- LTX.par
  tell "Conventional breakdown model, used e.g. in "
  () <- citet ["doi:10.1006/icar.1999.6245"]
  tell "."
  
  () <- LTX.par
  tell "Runaway breakdown model, proposed by "
  () <- citet ["doi:10.1016/0375-9601(92)90348-P","doi:10.1070/PU2001v044n11ABEH000939"]
  tell "."
  
  () <- LTX.par
  tell "Cross section data are taken from"
  () <- citet ["isbn:3-540-64296-X","isbn:3540653473","isbn:354044338X"]
  tell "."

  
  () <- LTX.par
  hayashiModelDoc
