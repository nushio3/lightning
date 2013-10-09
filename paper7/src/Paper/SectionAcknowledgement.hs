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
  esc [doc| I appreciate Shinichi Enami for his advices 
        on ice surface charge chemistry.
        |]
                         