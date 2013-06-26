module Control.Monad.Author.Label where

import Data.Typeable

data Label 
  = FromType !TypeRep 
  | FromStr !String
  | Ap !Label !Label
  deriving (Eq, Ord, Show)


(</) :: Show a => Label -> a -> Label
l </ x = Ap l (FromStr $ show x)