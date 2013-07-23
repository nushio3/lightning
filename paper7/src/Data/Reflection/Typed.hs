{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Reflection.Typed where

import qualified Data.Reflection as Ref
import           Data.Tagged


infix 1 `being`


type family ValTypeOf tag :: *
type Given tag = Ref.Given (Tagged tag (ValTypeOf tag))

being :: forall tag r. tag -> ValTypeOf tag -> (Given tag => r) -> r
being _ val = Ref.give (Tagged val)

the :: forall tag. Given tag => tag -> ValTypeOf tag
the _ = unTagged (Ref.given :: Tagged tag (ValTypeOf tag))



