{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Model.Disk.Hayashi where

import           Control.Monad.Author
import           Control.Monad.RWS (tell)
import           Data.Reflection.Typed
import           Data.Monoid ((<>))
import           Model.Concepts
import qualified Text.LaTeX as LTX
import qualified Text.LaTeX.Base.Class as LTX
import qualified Text.LaTeX.Packages.AMSMath as LTX
import qualified Text.LaTeX.Utils as LTX
import           UnitTyped
import           UnitTyped.SI
import           UnitTyped.SI.Constants hiding (pi)
import           UnitTyped.SI.Meta
import           UnitTyped.SI.Derived.Length
import           UnitTyped.SI.Derived.Time
import           UnitTyped.Synonyms
import qualified UnitTyped.NoPrelude as U

hayashiModelDoc :: Monad m =>  AuthorT m ()
hayashiModelDoc = do
  tell "Hayashi model is as follows."
  LTX.eqnarray $ do
    surfaceDensityGasDoc

innerRadius, outerRadius, snowlineRadius :: AU Double
innerRadius = 0.35  *| astronomicalUnit
outerRadius = 2.7   *| astronomicalUnit
snowlineRadius = 36 *| astronomicalUnit


surfaceDensityGas :: Given OrbitalRadius => GramPerCm2 Double
surfaceDensityGas =
  cutoff *|
   (1700 *| gram |/| square (centi meter)) |*|
   (fmap (**(-1.5)) $ r |/| (1 *| astronomicalUnit))
  where
    r :: AU Double
    r = the OrbitalRadius
    cutoff =
      sigmoid (val $ (r |-| innerRadius) |/| scaleHeight) *
      sigmoid (val $ (r |-| outerRadius) |/| scaleHeight)


densityGas :: (Given OrbitalRadius, Given ZCoordinate) => GramPerCm3 Double
densityGas = autoc $
  factor *| surfaceDensityGas |/| h
  where
    factor = (2*pi)**(-1/2)
           * exp(negate $ val (square z |/| (2 *| square h)))
    z = the ZCoordinate
    h = scaleHeight

surfaceDensityGasDoc :: Monad m => AuthorT m ()
surfaceDensityGasDoc = do
  tell $ LTX.sigmau <> LTX.autoParens "r" LTX.& "=" LTX.& ""
  tell $ 1.7e3 `LTX.times` (LTX.autoParens ("r" / ("1" <> LTX.mathrm "au")) ** (negate $ 3/2))
  tell $ LTX.mathrm ("g/cm" **2)

temperature :: Given OrbitalRadius => Double :| Kelvin
temperature = 280 *| kelvin |*|
   (fmap (**(-0.5)) $ (the OrbitalRadius) |/| (1 *| astronomicalUnit))

soundSpeed :: Given OrbitalRadius => CmPerSec Double
soundSpeed = U.sqrt $ autoc cssq
  where
    cssq = (kB |*| temperature) |/| (2.34 *| m_p)
      `as`  (square (centi meter) |/| square second)

orbitalAngularVelocity :: Given OrbitalRadius => Double :| Hertz
orbitalAngularVelocity =
  U.sqrt $ autoc $ g |*| m_p |/| cubic (the OrbitalRadius)

scaleHeight :: Given OrbitalRadius => AU Double
scaleHeight = autoc $ soundSpeed |/| orbitalAngularVelocity
  where r = the OrbitalRadius

sigmoid :: Double -> Double
sigmoid x = 1/(1+exp (negate x))
