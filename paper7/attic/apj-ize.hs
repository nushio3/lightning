

parseFigureFns :: String -> [String]
parseFigureFns = isEps . go
  where
    isEps = (==".eps") .reverse . take 4 . reverse

    go [] = []
    go ('{':xs) = go2 "" xs
    go (_:xs) = go xs

    go2 fn ('}':xs) = fn : go xs
    go2 fn (c:xs) = go2 (fn++[c]) xs

main :: IO ()
main = do
  src <- readFile "output/paper.tex"
  fns <- parseFigureFns src  
  mapM_ putStrLn fns
