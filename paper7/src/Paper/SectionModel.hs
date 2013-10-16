{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Paper.SectionModel where

import           Control.Monad.RWS
import           HereDocument (doc)
import           Model.Disk.Hayashi (hayashiModelDoc)
  
import           Text.Authoring


sectionModel :: MonadAuthoring s w m => m ()
sectionModel = do
  command1 "section" $ raw "Model"
  esc $ "We study the following three models of lightning: "
  
  environment "itemize" $ do

    raw "\\item[{\\tt [C]} ]"
    
    esc $ [doc| 
           Conventional breakdown model, widely used in meteorological context,
           and also adopted into astrophysical context e.g. by |]
    citet ["doi:10.1006/icar.1999.6245"]
    raw ".\n\n"


    raw "\\item[{\\tt [DP]} ]"

    esc $ [doc| 
           Druyversteyn-Penning breakdown model, based on electrical discharge 
           model proposed by |]
    citet ["doi:10.1103/RevModPhys.12.87"]
    esc " and introduced as a protoplanetary disk lightning model by "
    citet ["doi:10.1086/432796"]
    raw ".\n\n"


    raw "\\item[{\\tt [R]} ]"

    esc "Runaway breakdown model, proposed by "
    citet ["doi:10.1016/0375-9601(92)90348-P","doi:10.1070/PU2001v044n11ABEH000939"]
    raw ".\n\n"
  
  esc "Cross section data are taken from "
  citet ["isbn:3-540-64296-X","isbn:3540653473","isbn:354044338X"]
  raw ".\n\n"  


  hayashiModelDoc

