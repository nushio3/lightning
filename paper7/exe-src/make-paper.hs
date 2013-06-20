module Main where

import Development.Shake
import Development.Shake.FilePath
import System.Directory (setCurrentDirectory)
import System.Process (system)
import Text.Printf


workDir, unWorkDir :: FilePath
workDir = "dist/work"
unWorkDir = "../../"

main :: IO ()
main = do
  _ <- system $ printf "mkdir %s" workDir
  setCurrentDirectory workDir

  shakeArgs shakeOptions $ do
    want ["paper.pdf"]
    rulesExt
    rulesFiles

rulesExt :: Rules()
rulesExt = do
  "*.pdf" *> \out -> do
    let src = out -<.> "tex"
    need [src, "aastex.cls"]
    system' "pdflatex" [src]

rulesFiles :: Rules()
rulesFiles = do
  "paper.tex" *> \_out -> do
    copyFile' (unWorkDir </> "material/template.tex") "paper.tex"

  "*.cls" *> \out -> do
    let src = unWorkDir </> "material" </> out
    copyFile' src out