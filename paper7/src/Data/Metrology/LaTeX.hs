{-# LANGUAGE PolyKinds, DataKinds, TypeOperators, FlexibleInstances,
             ScopedTypeVariables, FlexibleContexts, ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

--

module Data.Metrology.LaTeX  where

import Data.Proxy (Proxy(..))
import Data.List
import Data.Singletons (Sing, sing, SingI)

import Data.Metrology.Internal -- am I an expert?
import Data.Metrology.Unsafe
import Data.Metrology.Z
import Data.Metrology

class ShowUnitFactor (dims :: [Factor *]) where
  showDims :: Proxy dims -> ([String], [String])

instance ShowUnitFactor '[] where
  showDims _ = ([], [])

instance (ShowUnitFactor rest, Show unit, SingI z)
         => ShowUnitFactor (F unit z ': rest) where
  showDims _ =
    let (nums, denoms) = showDims (Proxy :: Proxy rest)
        baseStr        = show (undefined :: unit)
        power          = szToInt (sing :: Sing z)
        abs_power      = abs power
        str            = if abs_power == 1
                         then baseStr
                         else baseStr ++ "^" ++ (show abs_power) in
    case compare power 0 of
      LT -> (nums, str : denoms)
      EQ -> (nums, denoms)
      GT -> (str : nums, denoms)

showFactor :: ShowUnitFactor dimspec => Proxy dimspec -> String
showFactor p
  = let (nums, denoms) = mapPair (build_string . sort) $ showDims p in
    case (length nums, length denoms) of
      (0, 0) -> ""
      (_, 0) -> " " ++ nums
      (0, _) -> " 1/" ++ denoms
      (_, _) -> " " ++ nums ++ "/" ++ denoms
  where
    mapPair :: (a -> b) -> (a, a) -> (b, b)
    mapPair f (x, y) = (f x, f y)

    build_string :: [String] -> String
    build_string [] = ""
    build_string [s] = s
    build_string s = "(" ++ build_string_helper s ++ ")"

    build_string_helper :: [String] -> String
    build_string_helper [] = ""
    build_string_helper [s] = s
    build_string_helper (h:t) = h ++ " * " ++ build_string_helper t

-- enable showing of compound units:
instance (Show u1, Show u2) => Show (u1 :* u2) where
  show _ = show (undefined :: u1) ++ " " ++ show (undefined :: u2)

instance (Show u1, Show u2) => Show (u1 :/ u2) where
  show _ = show (undefined :: u1) ++ "/" ++ show (undefined :: u2)

instance (Show u1, SingI power) => Show (u1 :^ (power :: Z)) where
  show _ = show (undefined :: u1) ++ "^" ++ show (szToInt (sing :: Sing power))

-- enable showing of units with prefixes:
instance (Show prefix, Show unit) => Show (prefix :@ unit) where
  show _ = show (undefined :: prefix) ++ show (undefined :: unit)

instance (ShowUnitFactor (LookupList dims lcsu), Show n)
           => Show (Qu dims lcsu n) where
  show (Qu d) = (show d ++ showFactor (Proxy :: Proxy (LookupList dims lcsu)))

infix 1 `showIn`

-- | Show a dimensioned quantity in a given unit. (The default @Show@
-- instance always uses canonical units.)
showIn :: ( ValidDLU dim lcsu unit
          , Fractional n
          , Show unit
          , Show n )
       => Qu dim lcsu n -> unit -> String
showIn x u = show (x # u) ++ " " ++ show u
