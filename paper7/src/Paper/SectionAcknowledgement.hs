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
  [rawQ| 
        We thank Yasuo Fukui, Hitoshi Miura, Munetake Momose and Takayuki Muto for helpful discussions; 
        Shinichi Enami for his advices on ice surface charge chemistry;
        Edward Kmett, Simon Peyton Jones and Richard Eisenberg for their comments on our Haskell programs;
        Tooru Eguchi for instruction of Japanese Virtual Observatory;
        Motomichi Tashiro for discussion on empirical formula of ion-molecule collision cross section.
        We are grateful to 
        Institute of Low Temperature Science, Hokkaido University for hosting the workshop
        ``Recent Development in Studies of Protoplanetary Disks with ALMA''
        where the authors learned ALMA image analysis.
        We used {\tt units} library @{citep ["special:units"]} to thoroughly check for the 
        consistency of physical dimensions and units in this paper.
        |]

                         