module Control.Monad.Author.State where

import Data.Default

data Root = Root deriving (Eq, Show)

instance Default Root where
  def = Root