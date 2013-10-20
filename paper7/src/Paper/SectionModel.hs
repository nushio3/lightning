{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Paper.SectionModel where

import           Control.Monad.RWS
import           Data.Dynamic
import           Model.Breakdown  
import           Model.Disk.Hayashi (hayashiModelDoc)

import           Text.Authoring
import           Text.Authoring.TH 


sectionModel :: MonadAuthoring s w m => m ()
sectionModel = do
  command1 "section" $ raw "Model"
  
  command1 "subsection" $ raw "Dielectric Strength of Air"  
  
  aboutDielectricStrengthOfAir

  command1 "subsection" $ raw "Breakdown Models"
  aboutThreeDischargeModel
  
  
  hayashiModelDoc  

