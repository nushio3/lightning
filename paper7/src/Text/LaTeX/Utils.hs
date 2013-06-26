module Text.LaTeX.Utils where

import Text.LaTeX 
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax


-- | A numbered mathematical eqnarray (or otherwise math expression).
eqnarray :: LaTeXC l => l -> l
eqnarray = liftL $ TeXEnv "eqnarray" []