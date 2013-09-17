{-# LANGUAGE TemplateHaskell #-}

module Text.LaTeX.Author.State where

import           Control.Lens.TH (makeLenses)
import           Data.Default
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Typeable
import qualified Text.CSL.Input.Identifier.Internal as Citation


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
  evalStateT (withDBFile fn go) def
  where
    go :: StateT DB m (a, AuthorState, LaTeX)
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
