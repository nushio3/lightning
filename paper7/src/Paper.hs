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
import           Text.LaTeX.Base.Syntax (LaTeX(..),TeXArg(..))
import           Text.LaTeX.Base.Writer (LaTeXT(..), execLaTeXT)
import           Text.LaTeX.Base.Class (liftL)
import qualified Text.LaTeX as LTX
import qualified Text.LaTeX.Packages.Graphicx as LTX
import           Text.LaTeX.Packages.AMSMath (autoParens)

import           Paper.SectionModel (sectionModel)
import           Paper.SectionObservation (sectionObservation)
import           Paper.SectionAcknowledgement (sectionAcknowledgement)
import           System.IO.Unsafe

import           HereDocument

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
abstract = TeXRaw "We study lightning distribution in protoplanetary disks."


genBodyText :: IO (Text.Text, Text.Text)
genBodyText = do
  let paper = do
        sectionIntro
        sectionModel
        sectionObservation
        sectionConclusion
        sectionAcknowledgement

  (_,as1,bodyTeX) <- withDatabaseFile "material/citation.db" paper



  return (LTX.render bodyTeX, bibText)

sectionIntro :: MonadAuthoring s w m => m ()
sectionIntro = do
  tell $ LTX.section "Introduction"
  tell [doc| Meteorites include unmodified materials from the protoplanetary disks that formed 
             our Solar System and carries unique evidences to understand stars and planet formation. |]
  () <- LTX.par
  tell [doc| Substantial progress has been made in the understanding of the lightning ignition mechanism 
             in these twenty years. |]
  tell "This work is based on the landmark review by "
  cite "isbn:9784130627184"
  tell ". The parallel computations are based on "
  cite "isbn:9781449335946"
  tell "."

sectionConclusion :: MonadAuthoring s w m => m ()
sectionConclusion = do
  tell $ LTX.section "Conclusion"
  tell $ "distribution was shown."
