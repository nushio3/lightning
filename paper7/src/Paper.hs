{-# LANGUAGE OverloadedStrings #-}
module Paper where

import           Control.Lens ((^.))
import           Control.Monad.Author
import qualified Control.Monad.Author.State as AS
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
import qualified Text.CSL as CSL
import qualified Text.CSL.Input.Identifier as CSL (db)
import           Text.LaTeX.Base.Syntax (LaTeX(..),TeXArg(..))
import           Text.LaTeX.Base.Writer (LaTeXT(..), execLaTeXT)
import           Text.LaTeX.Base.Class (liftL)
import qualified Text.LaTeX as LTX
import qualified Text.LaTeX.Packages.Graphicx as LTX
import           Text.LaTeX.Packages.AMSMath (autoParens)

import           Paper.SectionModel (sectionModel)
import           Paper.SectionAcknowledgement (sectionAcknowledgement)
import           System.IO.Unsafe

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
        sectionConclusion
        sectionAcknowledgement

  (_,as1,bodyTeX) <- runAuthorTWithDBFile "material/citation.db" paper
  
  let 
    bibentryOf :: String -> String
    bibentryOf url = case Map.lookup url (as1 ^. AS.citationDB . CSL.db) :: Maybe String of
      Nothing -> ""
      Just str -> let (s0,s1) = break (=='{') str
                      (_,s2) = break (==',') s1
                  in s0 ++ "{" ++ url ++ s2

    processed0 = map bibentryOf $ Set.toList $ as1 ^. AS.citedUrlSet
                  
    bibText = 
      Text.unlines $
      map Text.pack processed0                       


  return (LTX.render bodyTeX, bibText)

sectionIntro :: Monad m => AuthorT m ()
sectionIntro = do
  tell $ LTX.section "Introduction"
  tell $ "intro bra bra."

sectionConclusion :: Monad m => AuthorT m ()
sectionConclusion = do
  tell $ LTX.section "Conclusion"
  tell $ "distribution was shown."

