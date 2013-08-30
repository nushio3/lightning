{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Author.Cite where

import Control.Lens (use, (.=))
import Control.Monad.Author.State as AS
import Control.Monad.State (runStateT, MonadState)
import Control.Monad.IO.Class
import qualified Data.Text as Text
import Text.CSL.Input.Identifier(resolve)
import Text.CSL.Reference (emptyReference, Reference)
import Text.LaTeX.Base.Syntax as LTX
import Text.LaTeX.Base.Class as LTX
import Text.LaTeX.Base.Commands as LTX

cite :: (MonadState AS.Root m, MonadIO m, LaTeXC (m a)) => String -> m a
cite url = do
  db1 <- use citationDB
  (ref, db2) <- liftIO $ runStateT (resolve url) db1
  citationDB .= db2
  LTX.cite $ LTX.raw $ Text.pack url