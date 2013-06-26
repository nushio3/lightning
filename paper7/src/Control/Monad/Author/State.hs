{-# LANGUAGE TemplateHaskell #-}
module Control.Monad.Author.State where


import           Control.Monad.Author.Label (Label)
import           Control.Lens.TH (makeLenses)
import           Data.Default
import qualified Data.Map.Strict as Map

data Root 
  = Root 
  { _title :: String
  , _labelMap :: Map.Map Label String
  } deriving (Eq, Show)

$(makeLenses ''Root)

instance Default Root where
  def = Root def def