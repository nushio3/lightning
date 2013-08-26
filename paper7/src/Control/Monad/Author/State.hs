{-# LANGUAGE TemplateHaskell #-}
module Control.Monad.Author.State where


import           Control.Monad.Author.Label (Label)
import           Control.Lens.TH (makeLenses)
import           Data.Default
import           Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Text.CSL.Input.Identifier.Internal as Citation

data Root
  = Root
  { _labelMap :: Map.Map Label Text
  , _citationDB :: Citation.DB
  }

$(makeLenses ''Root)

instance Default Root where
  def = Root def def
