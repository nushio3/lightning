module Field where

import qualified Data.Array.Repa as R
import qualified Data.Map as M
import           Data.Word


type Field r e = R.Array r R.DIM2 e
type RGB = (Word8,Word8,Word8)
