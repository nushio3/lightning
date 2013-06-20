{-# LANGUAGE OverloadedStrings #-}
module Paper where


import           Control.Monad.Writer (execWriter, tell)
import           Data.Monoid ((<>))                 
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import           Text.LaTeX.Base.Syntax (LaTeX(..),TeXArg(..))
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
  sectionIntro <>
  sectionConclusion

sectionIntro :: LaTeX
sectionIntro = execWriter $ do
  tell $ TeX.section "Introduction"
  tell $ TeXRaw "intro bra bra."
  tell $ TeXEnv "eqnarray" [] $ execWriter $ do
    tell $ autoParens(x+y)*autoParens(1/x+y)
    where
      x = TeXRaw "x"
      y = TeXRaw "y"
sectionConclusion :: LaTeX
sectionConclusion = execWriter $ do
  tell $ TeX.section "Conclusion"
  tell $ TeXRaw "distribution was shown."