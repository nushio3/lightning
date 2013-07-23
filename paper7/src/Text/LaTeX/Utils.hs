module Text.LaTeX.Utils where

import Data.Text (pack)
import Text.LaTeX 
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax



texComm :: LaTeXC l => String -> [(LaTeX -> TeXArg, l)] -> l
texComm cmd xs = liftListL go $ map snd xs
  where 
    go :: [LaTeX] -> LaTeX
    go xs2 = TeXComm cmd $ zipWith ($) (map fst xs) xs2

texEnv :: LaTeXC l => String -> [(LaTeX -> TeXArg, l)] -> l -> l
texEnv cmd xs body = liftListL go (body : map snd xs)
  where 
    go :: [LaTeX] -> LaTeX
    go (body2 : xs2) = TeXEnv cmd (zipWith ($) (map fst xs) xs2) body2


citet :: LaTeXC l => String -> l
citet str = fromLaTeX $ TeXComm "citet" [FixArg $ raw $ pack $ str]


-- | A numbered mathematical eqnarray (or otherwise math expression).
eqnarray :: LaTeXC l => l -> l
eqnarray = texEnv "eqnarray" []



