{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Paper.SectionAcknowledgement where

import           Control.Monad.RWS
import           HereDocument (doc)

import           Text.Authoring

sectionAcknowledgement :: MonadAuthoring s w m => m ()
sectionAcknowledgement = do
  command1 "section*" $ raw "Acknowledgement"
  esc [doc| 
        We thank Edward Kmett, Simon Peyton Jones, Richard Eisenberg and Takayuki Muto for helpful discussions; 
        Shinichi Enami for his advices on ice surface charge chemistry;
        Eiji Akiyama, Hideko Nomura and Tooru Eguchi for the estimation of ALMA sensitivity.
        |]
                         