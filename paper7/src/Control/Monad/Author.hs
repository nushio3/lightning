{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Author where

import           Text.LaTeX (LaTeX)
import           Control.Monad.State.Strict (StateT(..), evalStateT, MonadState(..))
import           Control.Monad.Trans.Class (lift)
import           Text.LaTeX.Base.Writer (LaTeXT, execLaTeXT)

data AuthorState = AuthorState

type AuthorT m = LaTeXT (StateT AuthorState m)

runAuthorT :: Monad m => AuthorT m a -> m LaTeX
runAuthorT = flip evalStateT AuthorState . execLaTeXT

instance Monad m => MonadState AuthorState (AuthorT m) where
    get = lift get
    put = lift . put
    state = lift . state