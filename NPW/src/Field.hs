module Field where

import           Control.Monad.Identity
import qualified Data.Array.Repa as R
import           Data.Word


type Field r e = R.Array r R.DIM2 e
type System = (Field R.U Int,  Field R.U Double)

type RGB = (Word8,Word8,Word8)

type Edge = (R.DIM2, R.DIM2)

    
    
plot :: System -> Field R.U RGB                    
plot (bc, pot) =
  runIdentity $ R.computeUnboxedP $ R.zipWith go bc pot
  where
    go (-2) _ = (255,255,255)
    go (-1) _ = (0,0,64)
    go 1    _ = (64,0,0)
    go _    v = (rb(v*128+128),rb(128-v*128),255)
    rb = round . max 0 . min 255
    
    