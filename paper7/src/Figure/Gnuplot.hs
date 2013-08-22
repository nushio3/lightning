module Figure.Gnuplot(gnuplot) where

import System.Process (system)

gnuplot :: [String] -> IO ()
gnuplot cmds = do
  let tmpfn = "tmp.gnuplot"
  writeFile tmpfn $ unlines cmds
  system $ "gnuplot " ++ tmpfn
  return ()