module Model.Observation where

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Breakdown where


import Text.Authoring
import Text.Authoring.TH
import Data.Metrology
import Data.Metrology.SI
import Data.Metrology.Synonyms
import Data.Metrology.Show

sourceDistance :: Length
sourceDistance = 56 % Parsec



