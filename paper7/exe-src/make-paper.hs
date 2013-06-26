module Main where

import Development.Shake
import Development.Shake.FilePath
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Process (system)
import Text.Printf

import Paper(writePaper)


workDir :: FilePath
workDir = "dist/work"

main :: IO ()
main = do
  _ <- system $ printf "mkdir -p %s" workDir
  shakeArgs shakeOptions $ do
    want [workDir </> "paper.pdf"]
    rulesExt
    rulesFiles

rulesExt :: Rules()
rulesExt = do
  "//*.pdf" *> \out -> do
    let src = out -<.> "tex"
        cls = takeDirectory out </> "aastex.cls"
    need [src, cls]
    system' "pdflatex" ["-output-directory=" ++ workDir, "-halt-on-error", src]

rulesFiles :: Rules()
rulesFiles = do
  "//paper.tex" *> \out -> do
    let src = "./material/template.tex"
        exe = "./dist/build/make-paper/make-paper"
    need [src,exe]
    liftIO $ writePaper src out

  "dist//*.cls" *> \out -> do
    let src = "material" </> takeFileName out
    copyFile' src out