{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Control.Monad.Author.Type where

import           Control.Lens ((.~),(&),(^.))
import           Control.Monad
import qualified Control.Monad.Author.State as AS
import           Control.Monad.RWS
import           Control.Monad.State as State
import           Control.Monad.Trans.Class (lift)
import           Data.Default (def)
import           Data.String

import           Text.CSL.Input.Identifier (withDBFile, DB)
import           Text.LaTeX (LaTeX)
import           Text.LaTeX.Base.Class (LaTeXC(..))
import           Text.LaTeX.Base.Syntax (LaTeX)
import           Text.LaTeX.Base.Writer (LaTeXT, execLaTeXT)


type AuthorT = RWST () LaTeX AS.Root
type MonadAuthor = MonadRWS () LaTeX AS.Root

runAuthorTWithDBFile :: forall m a. (MonadIO m) => FilePath -> AuthorT m a -> m (a, AS.Root, LaTeX)
runAuthorTWithDBFile fn prog = do
  evalStateT (withDBFile fn go) def
  where
    go :: StateT DB m (a, AS.Root, LaTeX)
    go = do
       db1 <- State.get
       ret@(ret2, rootState2, doc2) <- lift $ runRWST prog () (def & AS.citationDB .~  db1)
       State.put (rootState2 ^. AS.citationDB)
       return ret
       

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
