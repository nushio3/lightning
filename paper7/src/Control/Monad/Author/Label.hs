module Control.Monad.Author.Label where

import Data.Typeable

data Label 
  = FromType !TypeRep 
  | FromStr !String
  | Ap !Label !Label
  deriving (Eq, Ord)

instance Show Label where
  show (FromType r) = show r
  show (FromStr s) = s
  show (Ap a b) = show a ++ show b

(</) :: Show a => Label -> a -> Label
l </ x = Ap l (FromStr $ show x)
