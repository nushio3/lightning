module Main where

import Control.Monad
import Development.Shake
import Development.Shake.FilePath
import Figure.Shake (rulesFigures)
import GlobalConfig(workDir)
import System.Directory (doesFileExist)
import System.Process (system)
import Text.Printf

import Paper.Slides(writePaper)


main :: IO ()
main = do
  _ <- system $ printf "mkdir -p %s" workDir
  shakeArgs shakeOptions $ do
    want [workDir </> "presentation.pdf"]
    rulesExt
    rulesFiles

rulesExt :: Rules()
rulesExt = do
  "//*.pdf" *> \out -> do
    let src = out -<.> "tex"
        heresrc = "presentation.tex"
        cls = takeDirectory out </> "aastex.cls"
        bib = takeDirectory out </> "presentation.bib"
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
      writeFile (workDir </> runnerFn) cmd
      system $ "chmod 755 " ++ (workDir </> runnerFn)
      system $ printf "cd %s; ./%s;" workDir runnerFn
      return ()

rulesFiles :: Rules ()
rulesFiles = do
  rulesFigures

  ["//presentation.tex", "//presentation.bib"] *>> \[outFn,bibFn] -> do
    let src = "./material/presentation-template.tex"
        exe = "./dist/build/make-presentation/make-presentation"
    need [src,exe]
    liftIO $ writePaper src outFn bibFn

  "dist//*.cls" *> \out -> do
    let src = "material" </> takeFileName out
    copyFile' src out

  "dist//*.bst" *> \out -> do
    let src = "material" </> takeFileName out
    copyFile' src out
