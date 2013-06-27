module Main where

import Control.Monad
import Development.Shake
import Development.Shake.FilePath
import System.Directory (getCurrentDirectory, setCurrentDirectory, doesFileExist)
import System.Process (system)
import Text.Printf

import Paper(writePaper)


workDir :: FilePath
workDir = "dist/output"

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
        heresrc = "paper.tex"
        cls = takeDirectory out </> "aastex.cls"
        bib = takeDirectory out </> "the.bib"
        bst = takeDirectory out </> "apj.bst" 
    need [src, cls, bib, bst]

    let 
      cmd = unlines $
        [ printf "pdflatex -halt-on-error %s" heresrc
        , printf "bibtex %s" heresrc
        , printf "pdflatex -halt-on-error %s" heresrc
        , printf "pdflatex -halt-on-error %s" heresrc
          ]
      runnerFn :: FilePath
      runnerFn = "run.sh"
    liftIO $ do
      writeFile (workDir </> runnerFn) cmd
      system $ "chmod 755 " ++ (workDir </> runnerFn)
      system $ printf "chdir %s; ./%s;" workDir runnerFn
      return ()

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
    
  "dist//*.bst" *> \out -> do
    let src = "material" </> takeFileName out
    copyFile' src out

  "dist//*.bib" *> \out -> do
    let src = "material" </> takeFileName out
        bibFn1 = "/home/nushio/My Library.bib"
        bibFn2 = "./material/the.bib"
    
    liftIO $ do
      b <- System.Directory.doesFileExist bibFn1
      when b $ do
        system $ printf "cp '%s' %s" bibFn1 bibFn2
        return ()
      when (not b) $ do
        printf "ARAHEN!"
    
    copyFile' bibFn2 out    