{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Paper.SectionCrossSection where

import           Control.Monad.RWS
import           Text.Authoring
import           Text.Authoring.TH

sectionCrossSection :: MonadAuthoring s w m => m ()
sectionCrossSection = do
  command1 "section*" $ raw "Cross Section Model of Ion-Neutral Molecular Collision"
  [rawQ| 
     We establish a model of collision cross section as functions of collision energy,
in collaboration with Motomichi Tashiro.
        |]

                         