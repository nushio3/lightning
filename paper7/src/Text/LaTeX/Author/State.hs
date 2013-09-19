{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.LaTeX.Author.State where

import           Control.Lens ((.~),(&),(^.))
import           Control.Lens.TH (makeLenses)
import           Control.Monad
import           Control.Monad.RWS
import           Control.Monad.State as State
import           Control.Monad.Trans.Class (lift)
import           Data.Default
import qualified Data.Map.Strict as Map
import           Data.Monoid
import qualified Data.Set as Set
import           Data.String
import           Data.Text (Text)
import           Data.Typeable
import           Text.LaTeX (LaTeX)
import           Text.LaTeX.Base.Class (LaTeXC(..))
import           Text.LaTeX.Base.Syntax (LaTeX)
import           Text.LaTeX.Base.Writer (LaTeXT, execLaTeXT)
import qualified Text.CSL.Input.Identifier.Internal as Citation
import           Unsafe.Coerce

-- | 'Label's are used to create a unique reference label
--   in a paper.
data Label 
  = FromType !TypeRep 
  | FromStr !String
  | Ap !Label !Label
  deriving (Eq, Ord)

instance Show Label where
  show (FromType r) = show r
  show (FromStr s) = s
  show (Ap a b) = show a ++ show b

infixl 1 ./

-- | Create a slightly different version of the label.
(./) :: Show a => Label -> a -> Label
l ./ x = Ap l (FromStr $ show x)


-- | 'AuthorState' maintains whatever information
--   needed to proceed the authoring of the body of a paper.
--   After completing the body part, 'AuthorState' is used 
--   for generating bibliographies, etc.

data AuthorState
  = AuthorState
  { _labelMap :: Map.Map Label Text
  , _citationDB :: Citation.DB
  , _citedUrlSet :: Set.Set String
  }

$(makeLenses ''AuthorState)

instance Default AuthorState where
  def = AuthorState def def def

-- | An RWST monad that writes a 'LaTeX' and keeps 'AuthorState'
type AuthorT = RWST () LaTeX AuthorState
-- | An RWST monad that writes a 'LaTeX' and keeps 'AuthorState'
type MonadAuthor = MonadRWS () LaTeX AuthorState

-- | given a citation database filename and an 'AuthorT' monad to run, 
--   performs the authoring, updates the citation database file,
--   
runAuthorTWithDBFile :: forall m a. (MonadIO m) => FilePath -> AuthorT m a -> m (a, AuthorState, LaTeX)
runAuthorTWithDBFile fn prog = do
  evalStateT (Citation.withDBFile fn go) def
  where
    go :: StateT Citation.DB m (a, AuthorState, LaTeX)
    go = do
       db1 <- State.get
       ret@(ret2, rootState2, doc2) <- lift $ runRWST prog () (def & citationDB .~  db1)
       State.put (rootState2 ^. citationDB)
       return ret
       

instance Monad m => Monoid (AuthorT m a) where
  mempty = return $ unsafeCoerce ()
  mappend = (>>)

instance Monad m => IsString (AuthorT m a) where
  fromString str = (tell $ fromString str) >> mempty

instance Monad m => LaTeXC (AuthorT m a) where
  liftListL f xs = do
    -- run the arguments piecewise, while updating the state and collecting the arguments.
    fragments <- forM xs $ \x -> do
      s0 <- get
      (_, s1, w1) <- lift $ runRWST x () s0      
      put s1
      return w1
    tell $ f fragments  
    mempty