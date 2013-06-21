{-# LANGUAGE OverloadedStrings #-}
module Paper where


import           Control.Monad.Author
import           Control.Monad.Writer (execWriter, tell)
import           Control.Monad.State.Strict (modify)
import           Data.Monoid ((<>))
import           Data.Functor.Identity
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import           Text.LaTeX.Base.Syntax (LaTeX(..),TeXArg(..))
import           Text.LaTeX.Base.Writer (LaTeXT(..), execLaTeXT)
import qualified Text.LaTeX as TeX
import qualified Text.LaTeX.Packages.Graphicx as TeX
import           Text.LaTeX.Packages.AMSMath (autoParens)

writePaper :: FilePath -> FilePath -> IO ()
writePaper srcFn outFn = do
  srcStr <- Text.readFile srcFn
  print $ (srcFn, outFn)
  Text.writeFile outFn $
    Text.unlines $
    map rep $ Text.lines srcStr
  where
    (<?) = Text.isInfixOf
    rep str
      | "Insert abstract here" <? str      = abstractText
      | "Insert document body here" <? str = bodyText
      | otherwise                          = str

abstractText :: Text.Text
abstractText = TeX.render abstract

abstract :: LaTeX
abstract = TeXRaw "We study lightning distribution in protoplanetary disks."

bodyText :: Text.Text
bodyText = TeX.render $
  runIdentity $ runAuthorT $ do
    sectionIntro
    sectionConclusion

sectionIntro :: Monad m => AuthorT m ()
sectionIntro = do
  TeX.section "Introduction"
  "intro bra bra."
  TeX.liftL $ TeXEnv "eqnarray" [] $ do
    autoParens(x+y)*autoParens(1/x+y)

  where
      x = "x"
      y = "y"

sectionConclusion :: Monad m => AuthorT m ()
sectionConclusion = do
  TeX.section "Conclusion"
  modify (id :: AuthorState ->  AuthorState)
  "distribution was shown."