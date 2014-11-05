{-# LANGUAGE OverloadedStrings #-}

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Printf
import System.Process

parseFigureFns :: String -> [String]
parseFigureFns = filter isEps . go
  where
    isEps = (==".eps") .reverse . take 4 . reverse

    go [] = []
    go ('{':xs) = go2 "" xs
    go (_:xs) = go xs

    go2 fn ('}':xs) = fn : go xs
    go2 fn (c:xs) = go2 (fn++[c]) xs

newNames :: [String] -> [(String, String)]
newNames fns = go 0 'a' '?' fns
  where
    go _ _ _ [] = []
    go n c m (x:xs) = (x,newFn n' c'): go n' c' m' xs
      where
        m' = x!!7
        (n',c') = if (m/=m') then (succ n,'a')
                             else (n,succ c)
        newFn :: Int -> Char -> String
        newFn n' c' = printf "fig%d%c.eps" n' c'

applyRename :: [(String, String)] -> T.Text -> T.Text
applyRename rs str = go (map p2 rs) (str)
  where
    p2 (a,b) = (T.pack a, T.pack b)

    go [] txt = txt
    go ((a,b):xs) txt = T.replace a b $ go xs txt


copyFig (a,b) = system $ printf "cp -f output/%s pub-apj/%s" a b

main :: IO ()
main = do
  src <- readFile "output/paper.tex"
  bibcon <- T.readFile "output/paper.bbl"
  let fns = parseFigureFns src  
      renameList = newNames fns
  let src' = 
        T.replace "\\bibliography{the.bib}" bibcon $
        applyRename renameList (T.pack src)
  system "mkdir -p pub-apj"
  writeFile "pub-apj/main.tex" (T.unpack src')
  mapM_ copyFig renameList
  system "cp -f output/aastex.cls  output/apj.bst pub-apj/"
  return ()
