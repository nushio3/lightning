{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Paper.SectionAcknowledgement where

import           Control.Monad.Author
import qualified Text.LaTeX as LTX
import           Text.LaTeX.Base.Syntax(TeXArg(..))
import qualified Text.LaTeX.Utils as LTX
import           HereDocument (doc)

sectionAcknowledgement :: forall m. Monad m => AuthorT m ()
sectionAcknowledgement = do
  LTX.texComm "section*" [(FixArg, "Acknowledgement")]                       
  [doc| I appreciate Shinichi Enami for his advices 
        on ice surface charge chemistry.
        |]
                         