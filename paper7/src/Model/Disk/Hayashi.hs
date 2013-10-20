{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Model.Disk.Hayashi where

import           Control.Monad.Identity
import           Control.Monad.RWS (tell)
import           Control.Monad.IO.Class
import           Data.Reflection.Typed
import           Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Text.LaTeX as LTX
import qualified Text.LaTeX.Base.Class as LTX
import qualified Text.LaTeX.Base.Commands as LTX
import qualified Text.LaTeX.Packages.AMSMath as LTX
import           UnitTyped
import           UnitTyped.SI
import           UnitTyped.SI.Constants hiding (pi)
import           UnitTyped.SI.Meta
import           UnitTyped.SI.Derived.Length
import           UnitTyped.SI.Derived.Mass
import           UnitTyped.SI.Derived.Time
import           UnitTyped.Synonyms
import qualified UnitTyped.NoPrelude as U

import           Model.Concepts
import           Model.Constants

import           Text.Authoring

hayashiModelDoc :: MonadAuthoring s w m => m ()
hayashiModelDoc = do
  citet1 "bibcode:1981PThPS..70...35H"
  
  esc " has proposed the following model of the protoplanetary disk."
  raw "\n\n"
  
  
  environment "eqnarray" $ do
    surfaceDensityGasDoc

innerRadius, outerRadius, snowlineRadius, innerSH, outerSH, snowlineSH :: AU Double
innerRadius = 0.35  *| astronomicalUnit
outerRadius = 36   *| astronomicalUnit
snowlineRadius = 2.7 *| astronomicalUnit

innerSH = OrbitalRadius `being` innerRadius $ scaleHeight
outerSH = OrbitalRadius `being` outerRadius $ scaleHeight
snowlineSH = OrbitalRadius `being` snowlineRadius $ scaleHeight


surfaceDensityGas :: Given OrbitalRadius => GramPerCm2 Double
surfaceDensityGas =
  cutoff *|
   (1700 *| gram |/| square (centi meter)) |*|
   (fmap (**(-1.5)) $ r |/| (1 *| astronomicalUnit))
  where
    r :: AU Double
    r = the OrbitalRadius
    cutoff =
      sigmoid (val $ (r |-| innerRadius) |/| innerSH) *
      sigmoid (negate $ val $ (r |-| outerRadius) |/| outerSH)

surfaceDensityGasDoc :: MonadAuthoring s w m => m ()
surfaceDensityGasDoc =  
  (latex =<<) $ LTX.execLaTeXT $ do
     LTX.sigmau <> LTX.autoParens "r" LTX.& "=" LTX.& ""
     1.7e3 `LTX.times` (LTX.autoParens ("r" / ("1" <> LTX.mathrm "au")) ** (negate $ 3/2))
     LTX.mathrm ("g/cm" **2)



densityGas :: (Given OrbitalRadius, Given ZCoordinate) => GramPerCm3 Double
densityGas = autoc $
  factor *| surfaceDensityGas |/| h
  where
    factor = (2*pi)**(-1/2)
           * (exp(negate $ val (square z |/| (2 *| square h))))
    z = the ZCoordinate
    h = scaleHeight


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
  U.sqrt $ autoc $ g |*| (2e33 *| gram) |/| cubic (the OrbitalRadius)

orbitalVelocity :: Given OrbitalRadius => CmPerSec Double
orbitalVelocity =
  U.sqrt $ autoc $ g |*| (2e33 *| gram) |/| the OrbitalRadius



scaleHeight :: Given OrbitalRadius => AU Double
scaleHeight = autoc $ soundSpeed |/| orbitalAngularVelocity
  where r = the OrbitalRadius

sigmoid :: Double -> Double
sigmoid x = 1/(1+exp (negate x))

gaussian :: Double -> Double -> Double -> Double
gaussian mu sigma x = 1/sqrt(2*pi* sigma^2) * exp (negate $ (/2) $ ((x-mu)/sigma)^2)

pvDistribution :: AU Double -> CmPerSec Double -> Double
pvDistribution r v = gaussian (val v0) (val cs) (val v)
  where
    cs :: CmPerSec Double
    cs = OrbitalRadius `being` r0 $ soundSpeed

    v0 :: CmPerSec Double
    v0 = (sin inclination * signum (val r)) *| (OrbitalRadius `being` r0 $ orbitalVelocity)
    r0 :: AU Double
    r0 = U.abs r

chargedPVDistribution :: CmPerSec Double -> AU Double -> CmPerSec Double -> Double
chargedPVDistribution vch r v = gaussian (val v0) (val cs) (val v)
  where
    cs :: CmPerSec Double
    cs = OrbitalRadius `being` r0 $ soundSpeed |+| vch'

    vch'
      | val r0 < 100 = vch
      | otherwise    = fmap (* ((val r0 /100)**(-1.5))) vch

    v0 :: CmPerSec Double
    v0 = (sin inclination * signum (val r)) *| (OrbitalRadius `being` r0 $ orbitalVelocity)
    r0 :: AU Double
    r0 = U.abs r

relativePVDistribution :: CmPerSec Double -> AU Double -> CmPerSec Double -> Double
relativePVDistribution vch r v = chargedPVDistribution vch r v - pvDistribution r v

inclination :: Double
inclination = 7/180*pi