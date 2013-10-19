{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Paper.SectionAcknowledgement where

import           Control.Monad.RWS
import           Text.Authoring
import           Text.Authoring.TH

sectionAcknowledgement :: MonadAuthoring s w m => m ()
sectionAcknowledgement = do
  command1 "section*" $ raw "Acknowledgement"
  [escQ| 
        We thank Hitoshi Miura and Takayuki Muto for helpful discussions; 
        Shinichi Enami for his advices on ice surface charge chemistry;
        Edward Kmett, Simon Peyton Jones and Richard Eisenberg for their comments on our Haskell programs;
        Eiji Akiyama, Hideko Nomura and Tooru Eguchi for the estimation of ALMA sensitivity.
        |]
                         