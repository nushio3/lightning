{-# LANGUAGE OverloadedStrings #-}
module Paper where


import           Control.Monad.Author
import           Control.Monad.RWS
import           Control.Monad.State.Strict (modify)
import           Data.Monoid ((<>))
import           Data.Functor.Identity
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import           Text.LaTeX.Base.Syntax (LaTeX(..),TeXArg(..))
import           Text.LaTeX.Base.Writer (LaTeXT(..), execLaTeXT)
import           Text.LaTeX.Base.Class (liftL)
import qualified Text.LaTeX as LTX
import qualified Text.LaTeX.Packages.Graphicx as LTX
import           Text.LaTeX.Packages.AMSMath (autoParens)

import           Paper.SectionModel (sectionModel)
import           Paper.SectionAcknowledgement (sectionAcknowledgement)

writePaper :: FilePath -> FilePath -> IO ()
writePaper srcFn outFn = do
  srcStr <- Text.readFile srcFn
  print $ (srcFn, outFn)

  bodyText <- genBodyText

  let
    (<?) = Text.isInfixOf
    rep str
      | "Insert abstract here" <? str      = abstractText
      | "Insert document body here" <? str = bodyText
      | otherwise                          = str

  Text.writeFile outFn $
    Text.unlines $
    map rep $ Text.lines srcStr

abstractText :: Text.Text
abstractText = LTX.render abstract

abstract :: LaTeX
abstract = TeXRaw "We study lightning distribution in protoplanetary disks."

genBodyText :: IO Text.Text
genBodyText = do
  (_,_,bodyTeX) <- runAuthorTWithDBFile "material/citation.db" go
  return $ LTX.render $ bodyTeX
  where
    go = do
      sectionIntro
      sectionModel
      sectionConclusion
      sectionAcknowledgement

sectionIntro :: Monad m => AuthorT m ()
sectionIntro = do
  tell $ LTX.section "Introduction"
  tell $ "intro bra bra."

sectionConclusion :: Monad m => AuthorT m ()
sectionConclusion = do
  tell $ LTX.section "Conclusion"
  tell $ "distribution was shown."

