{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Paper.Slides where

import           Control.Lens ((^.))
import           Control.Monad.RWS
import           Control.Monad.State.Strict (modify)
import           Data.Default (def)
import           Data.List (break)
import           Data.Monoid ((<>))
import           Data.Functor.Identity
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import           Text.Authoring
import           Text.Authoring.TH
import           Text.LaTeX.Base.Syntax (LaTeX(..),TeXArg(..))
import           Text.LaTeX.Base.Writer (LaTeXT(..), execLaTeXT)
import qualified Text.LaTeX as LTX

import           Paper.SectionModel (sectionModel)
import           Paper.SectionObservation (sectionObservation)
import           Paper.SectionAcknowledgement (sectionAcknowledgement)


writePaper :: FilePath -> FilePath -> FilePath -> IO ()
writePaper srcFn outFn bibFn = do
  srcStr <- Text.readFile srcFn
  print $ (srcFn, outFn)

  (bodyText,bibText) <- genBodyText

  let
    (<?) = Text.isInfixOf
    rep str
      | "Insert document body here" <? str = bodyText
      | otherwise                          = str

  Text.writeFile outFn $
    Text.unlines $
    map rep $ Text.lines srcStr

  Text.writeFile bibFn bibText


genBodyText :: IO (Text.Text, Text.Text)
genBodyText = do
  (bibText, _ , bodyDoc) <- runAuthoringT $ do
    withDatabaseFile "material/citation.db" sectionSlides
    txt <- bibliographyContent
    return txt



  return (LTX.render bodyDoc, bibText)

sectionSlides :: MonadAuthoring s w m => m ()
sectionSlides = do
  command1 "section" $ raw "Method"
  environment "frame" $ do
    environment "itemize" $ do
      raw "\\item Richard \\pause"
      raw "\\item Takayuki \\pause"      
  environment "frame" $ do
    environment "itemize" $ do
      raw "\\item meter \\pause"            
      raw "\\item kg \\pause"            
      raw "\\item second \\pause"            
      
