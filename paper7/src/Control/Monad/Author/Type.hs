{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Author.Type where

import           Text.LaTeX (LaTeX)
import           Control.Monad.State.Strict (StateT(..), evalStateT, MonadState(..))
import           Control.Monad.Trans.Class (lift)
import           Data.Default (def)
import           Text.LaTeX.Base.Writer (LaTeXT, execLaTeXT)
import qualified Control.Monad.Author.State as AuthorState


type AuthorT m = LaTeXT (StateT AuthorState.Root m)

runAuthorT :: Monad m => AuthorT m a -> m LaTeX
runAuthorT = flip evalStateT def . execLaTeXT

instance Monad m => MonadState AuthorState.Root (AuthorT m) where
    get = lift get
    put = lift . put
    state = lift . state