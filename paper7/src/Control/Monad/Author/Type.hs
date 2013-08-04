{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Control.Monad.Author.Type where

import           Control.Monad
import qualified Control.Monad.Author.State as AuthorState
import           Control.Monad.RWS
import           Control.Monad.Trans.Class (lift)
import           Data.Default (def)
import           Data.String

import           Text.LaTeX (LaTeX)
import           Text.LaTeX.Base.Class (LaTeXC(..))
import           Text.LaTeX.Base.Syntax (LaTeX)
import           Text.LaTeX.Base.Writer (LaTeXT, execLaTeXT)


type AuthorT = RWST () LaTeX AuthorState.Root
type MonadAuthor = MonadRWS () LaTeX AuthorState.Root

runAuthorT :: Monad m => AuthorT m a -> m (a, AuthorState.Root, LaTeX)
runAuthorT prog = runRWST prog () def

instance Monad m => Monoid (AuthorT m ()) where
  mempty = return ()
  mappend = (>>)

instance Monad m => IsString (AuthorT m ()) where
  fromString = tell . fromString  

instance Monad m => LaTeXC (AuthorT m ()) where
  liftListL f xs = do
    -- run the arguments piecewise, while updating the state and collecting the arguments.
    fragments <- forM xs $ \x -> do
      s0 <- get
      (_, s1, w1) <- lift $ runRWST x () s0      
      put s1
      return w1
    tell $ f fragments  