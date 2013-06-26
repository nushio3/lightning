{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Author.Ref where


import           Control.Applicative
import           Control.Lens.Getter (use)
import qualified Control.Monad.Author.State as AS
import           Control.Monad.Author.Label (Label)
import           Control.Monad.State.Strict (MonadState(..))
import           Text.LaTeX (rendertex)
import qualified Text.LaTeX as LTX
import           Text.LaTeX.Base.Class (LaTeXC(..))
import qualified Data.Map.Strict as Map

ref :: (MonadState AS.Root m, LaTeXC (m a), Functor m) => Label -> m a
ref lab = do
  ret <- Map.lookup lab <$> use AS.labelMap
  case ret of
    Just str -> LTX.ref $ rendertex str
    Nothing -> undefined