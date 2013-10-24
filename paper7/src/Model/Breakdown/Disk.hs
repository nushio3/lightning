{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Breakdown.Disk where

import Text.Authoring
import Text.Authoring.TH

import UnitTyped.Synonyms

import Model.Gas

aboutDiskDischarge :: MonadAuthoring s w m => m ()
aboutDiskDischarge = 
  [rawQ| 
In the MMSN model the gas density at eqatorial plane, $r = 1{\rm au}$ is $#{ppValE 2 ppdDensity} {\rm g~cm^{ -3}}$.
@{citep["bibcode:1989GeCoA..53..197A"]} 
|]
