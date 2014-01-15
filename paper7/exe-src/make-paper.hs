module Main where

import Control.Monad
import Development.Shake
import Development.Shake.FilePath
import Figure.Shake (rulesFigures)
import GlobalConfig
import System.Directory (doesFileExist)
import System.Process (system)
import Text.Printf

import Paper(writePaper)
main :: IO ()
main = do
  _ <- system $ printf "mkdir -p %s" workDir
  _ <- system $ printf "mkdir -p %s" figureDir
  shakeArgs shakeOptions $ do
    want [workDir </> "paper.pdf"]
    rulesExt
    rulesFiles

rulesExt :: Rules()
rulesExt = do
  "//*.pdf" *> \out -> do
    let src = out -<.> "tex"
        heresrc = "paper.tex"
        cls = takeDirectory out </> "aastex.cls"
        bib = takeDirectory out </> "the.bib"
        bst = takeDirectory out </> "apj.bst"
    need [src, cls, bib, bst]
    need $ map (printf "%s/fig%d.eps" workDir :: Int -> String) [1..1]

    let
      cmd = unlines $
        [ printf "pdflatex -halt-on-error %s" heresrc
        , printf "bibtex paper"
        , printf "pdflatex -halt-on-error %s" heresrc
        , printf "pdflatex -halt-on-error %s" heresrc
          ]
      runnerFn :: FilePath
      runnerFn = "run.sh"
    liftIO $ do
      system $ "./dist/build/make-figures/make-figures"
      writeFile (workDir </> runnerFn) cmd
      system $ "chmod 755 " ++ (workDir </> runnerFn)
      system $ printf "cd %s; ./%s;" workDir runnerFn
      return ()

rulesFiles :: Rules ()
rulesFiles = do
  rulesFigures

  ["//paper.tex", "//the.bib"] *>> \[outFn,bibFn] -> do
    let src = "./material/template.tex"
        exe = "./dist/build/make-paper/make-paper"
    need [src,exe]
    liftIO $ writePaper src outFn bibFn

  "dist//*.cls" *> \out -> do
    let src = "material" </> takeFileName out
    copyFile' src out

  "dist//*.bst" *> \out -> do
    let src = "material" </> takeFileName out
    copyFile' src out
