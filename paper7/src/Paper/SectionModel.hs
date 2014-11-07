
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Paper.SectionModel where

import           Control.Monad.RWS
import           Data.Dynamic
import           Model.Breakdown  
import           Model.Breakdown.Disk (aboutDiskDischarge)
import           Model.Disk.Hayashi (hayashiModelDoc)
import           Model.Disk.Recent (aboutLatestDiskModel)
import           Model.Breakdown (aboutAir)

import           Text.Authoring
import           Text.Authoring.TH 

sectionModel :: MonadAuthoring s w m => m ()
sectionModel = do
  command1 "section" $ raw "Model"
  
  command1 "subsection" $ raw "Dielectric Strength of Air"  

  [rawQ| xxx: Motivate air, or remove air. |]
  
  aboutDielectricStrengthOfAir

  command1 "subsection" $ raw "Breakdown Models on Earth"
  aboutThreeDischargeModel
  
  raw "\n\n"
  
  aboutAir
  
  command1 "subsection" $ raw "The Disk Model"  

  aboutLatestDiskModel

  hayashiModelDoc  



  command1 "subsection" $ raw "Breakdonw Models on Protoplanetary Disks"  

  aboutDiskDischarge