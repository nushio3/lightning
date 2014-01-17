{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Paper where

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
      | "Insert abstract here" <? str      = abstractText
      | "Insert document body here" <? str = bodyText
      | otherwise                          = str

  Text.writeFile outFn $
    Text.unlines $
    map rep $ Text.lines srcStr

  Text.writeFile bibFn bibText

abstractText :: Text.Text
abstractText = LTX.render abstract

abstract :: LaTeX
abstract = TeXRaw "We study the possibility of observationally distinguishing lightning models in protoplanetary disks."


genBodyText :: IO (Text.Text, Text.Text)
genBodyText = do
  let paper = do
        sectionIntro
        sectionModel
        sectionObservation
        sectionConclusion
        sectionAcknowledgement

  (bibText, _ , bodyDoc) <- runAuthoringT $ do
    withDatabaseFile "material/citation.db" paper
    txt <- bibliographyContent
    return txt



  return (LTX.render bodyDoc, bibText)

sectionIntro :: MonadAuthoring s w m => m ()
sectionIntro = do
  command1 "section" $ raw "Introduction"
  
  let takahashi2007 = citet ["isbn:9784130627184"]
      marlow2013    = citet ["isbn:9781449335946"]
  [escQ| 
Meteorites include unmodified materials from the protoplanetary disks that formed 
our Solar System and carries unique evidences to understand stars and planet formation. 
Substantial progress has been made in the understanding of the lightning ignition mechanism 
in these twenty years.

This work is based on the landmark review by @{takahashi2007}. 
The parallel computations are based on @{marlow2013}.

 |]

sectionConclusion :: MonadAuthoring s w m => m ()
sectionConclusion = do
  command1 "section" $ esc "Conclusions and Discussions."
  [escQ|   
   We have shown that some
   lightning models are observationally distinguishable with ALMA.
   It is possible to reject some of the lightning models based on ground observations.
   Our lightning models treated here are very simple. To apply this work to more realistic
   disk models are beyond the scope of this paper, and left as a useful direction
   for future research.

   |]
