module Text.LaTeX.Utils where

import Data.Text (pack)
import Text.LaTeX 
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax


-- | A numbered mathematical eqnarray (or otherwise math expression).
eqnarray :: LaTeXC l => l -> l
eqnarray = liftL $ TeXEnv "eqnarray" []

citet :: LaTeXC l => String -> l
citet str = fromLaTeX $ TeXComm "citet" [FixArg $ raw $ pack $ str]
