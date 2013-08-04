{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Control.Monad.Author.Type where

import           Text.LaTeX (LaTeX)
import           Control.Monad.RWS
import           Control.Monad.Trans.Class (lift)
import           Data.Default (def)
import           Text.LaTeX.Base.Syntax (LaTeX)
import           Text.LaTeX.Base.Writer (LaTeXT, execLaTeXT)
import qualified Control.Monad.Author.State as AuthorState


type AuthorT = RWST () LaTeX AuthorState.Root
type MonadAuthor = MonadRWS () LaTeX AuthorState.Root

runAuthorT :: Monad m => AuthorT m a -> m (a, AuthorState.Root, LaTeX)
runAuthorT prog = runRWST prog () def

