{-# LANGUAGE PolyKinds, DataKinds, TypeOperators, FlexibleInstances,
             ScopedTypeVariables, FlexibleContexts, ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

--

module Data.Metrology.LaTeX (ppIn, ppE, ppF, ppFP) where

import Data.Proxy (Proxy(..))
import Data.List
import Data.Singletons (Sing, sing, SingI)

import Data.Metrology.Internal -- am I an expert?
import Data.Metrology.Unsafe
import Data.Metrology.Z
import Data.Metrology
import Data.Metrology.Synonyms
import Data.Metrology.SI.Units

import Text.Printf

class LaTeXUnitFactor (dims :: [Factor *]) where
  renderDims :: Proxy dims -> ([String], [String])

instance LaTeXUnitFactor '[] where
  renderDims _ = ([], [])

instance (LaTeXUnitFactor rest, Show unit, SingI z)
         => LaTeXUnitFactor (F unit z ': rest) where
  renderDims _ =
    let (nums, denoms) = renderDims (Proxy :: Proxy rest)
        baseStr        = show (undefined :: unit)
        power          = szToInt (sing :: Sing z)
        abs_power      = abs power
        str            = if abs_power == 1
                         then baseStr
                         else baseStr ++ "^{" ++ (show abs_power) ++ "}" in
    case compare power 0 of
      LT -> (nums, str : denoms)
      EQ -> (nums, denoms)
      GT -> (str : nums, denoms)

renderFactor :: LaTeXUnitFactor dimspec => Proxy dimspec -> String
renderFactor p
  = let (nums, denoms) = mapPair (build_string . sort) $ renderDims p in
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
    build_string_helper (h:t) = h ++ " \\cdot " ++ build_string_helper t


-- the classes of types we know how to render it in LaTeX
class Render a where render :: a -> String

-- enable rendering of compound units:
instance (Render u1, Render u2) => Render (u1 :* u2) where
  render _ = render (undefined :: u1) ++ " " ++ render (undefined :: u2)

instance (Render u1, Render u2) => Render (u1 :/ u2) where
  render _ = render (undefined :: u1) ++ "/" ++ render (undefined :: u2)

instance (Render u1, SingI power) => Render (u1 :^ (power :: Z)) where
  render _ = render (undefined :: u1) ++ "^{" ++ show (szToInt (sing :: Sing power)) ++ "}"

-- enable rendering of units with prefixes:
instance (Show prefix, Render unit) => Render (prefix :@ unit) where
  render _ = show (undefined :: prefix) ++ render (undefined :: unit)

instance (LaTeXUnitFactor (LookupList dims lcsu), Render n)
           => Render (Qu dims lcsu n) where
  render (Qu d) = (render d ++ renderFactor (Proxy :: Proxy (LookupList dims lcsu)))

instance Render ElectronVolt where render = show
instance Render Meter where render = show
instance Render Volt where render = show                            

-- | Render a dimensioned quantity with a given unit, with the default numerical representaion.
ppIn :: ( ValidDLU dim lcsu unit
          , Fractional n
          , Render unit
          , Show n )
       => unit -> Qu dim lcsu n ->  String
ppIn u x  = show (x # u) ++ " {\\rm " ++ render u ++ "}"

-- | Render a dimensioned quantity with a given unit and given digits of precision, adequetly using the powers of 10.
ppE :: ( ValidDLU dim lcsu unit
          , Fractional n
          , Render unit
          , PrintfArg n ) => unit ->  Int -> Qu dim lcsu n  -> String
ppE u digits x = ret ++ " {\\rm " ++ render u ++ "}"
  where
    fmtStr :: String
    fmtStr = printf "%%.%de" digits
    
    protoStr :: String
    protoStr = printf fmtStr (x#u)

    (valPart,expPart) = break (=='e') protoStr
    
    ret = case expPart of
      "e0" -> valPart
      _ -> printf "%s \\times 10^{%s}" valPart (drop 1 expPart)


-- | Render a dimensioned quantity with a given unit, in given format, at the given powers of 10. It is sometimes useful to align the powers.
ppFP :: ( ValidDLU dim lcsu unit
          , Fractional n
          , Render unit
          , PrintfArg n ) => unit -> String -> Int -> Qu dim lcsu n  -> String
ppFP u fmtStr power10 x = ret ++ " {\\rm " ++ render u ++ "}"
  where
    protoStr :: String
    protoStr = printf fmtStr ((x#u) / 10^^power10) 

    ret =  printf "%s \\times 10^{%d}" protoStr power10




-- | Render a dimensioned quantity with a given unit, using the given printf formatter.
ppF :: ( ValidDLU dim lcsu unit
          , Fractional n
          , Render unit
          , PrintfArg n ) => unit ->  String -> Qu dim lcsu n  -> String
ppF u fmtStr x = protoStr ++ " {\\rm " ++ render u ++ "}"
  where
    protoStr :: String
    protoStr = printf fmtStr (x#u)
