main :: IO ()
main = do
  con <- getContents
  parse con
  
parse :: String -> IO ()
parse con = do
  mapM_ putStrLn $ filter isHeadline lc
  where
    lc :: [String]
    lc = lines con
    
    isHeadline :: String -> Bool
    isHeadline = 
      not . null .
      filter (not . flip elem "0123456789. \t")