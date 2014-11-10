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
  let takahashi2007 = citet ["isbn:9784130627184"]
      marlow2013    = citet ["isbn:9781449335946"]

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
        This work is based on the landmark review of terrestrial lightning by @{takahashi2007}. 
        The parallel computation techniques used in this work are based on @{marlow2013}.
        We used {\tt units} library @{citep ["special:units"]} to thoroughly check for the 
        consistency of physical dimensions and units in this paper.
        This work is supported by Grants-in-Aid for Scientific Research
        (\#23103005,\#24103506,\#25887023,\#26400224) from MEXT.
        This research used computational resources of the K computer provided by the RIKEN Advanced Institute for Computational Science(AICS). We thank RIKEN AICS for the support in conducting this research.
        |]

                         
