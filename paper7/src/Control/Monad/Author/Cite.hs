module Control.Monad.Author.Cite where

import Control.Lens (use)
import Control.Monad.State (runStateT)
import Control.Monad.IO.Class
import Text.CSL.Input.Identifier(resolve)
import Text.CSL.Reference (emptyReference, Reference)

cite :: (MonadState AS.Root m, MonadIO m, LaTeXC (m a)) => String -> m a
cite url = do
  db1 <- use (citationDB)
  (db2, ref) <- liftIO $ runStateT (resolve url) db1