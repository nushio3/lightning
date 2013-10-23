{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
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
import           UnitTyped.SI.Derived.Length
import           UnitTyped.SI.Derived.Time
import           UnitTyped.SI.Meta
import           UnitTyped.Synonyms
import qualified UnitTyped.NoPrelude as U

import           Model.Concepts
import           Model.Values

import           Text.Authoring
import           Text.Authoring.TH

hayashiModelDoc :: MonadAuthoring s w m => m ()
hayashiModelDoc = do
  citet1 "bibcode:1981PThPS..70...35H"
  
  esc " has proposed the following model of the protoplanetary disk."
  raw "\n\n"
  
  
  environment "eqnarray" $ do
    surfaceDensityGasDoc
    raw ",\\\\"
    temperatureDoc
    raw "."

  [rawQ| The assumption of the hydrostatic equilibrium leads to the vertical distribution of the gas
\begin{eqnarray}
\rho(r,z)&=&\rho_0(r) \exp \left(-\frac{z^2}{2h^2}\right), \\
\mathrm{where} ~~~ 
h(r) &=& \frac{c_s}{\Omega}, \\
c_s(r) &=& \sqrt{\frac{k_B T(r)}{\mu m_p}},\\
\Omega(r) &=& \sqrt{\frac{G M_{\odot}}{r^3}}.
\end{eqnarray}
   |]

innerRadius, outerRadius, snowlineRadius, innerSH, outerSH, snowlineSH :: AU Double
innerRadius =    mkVal 0.35  
outerRadius =    mkVal 300   
snowlineRadius = mkVal 2.7

innerSH = OrbitalRadius `being` innerRadius $ scaleHeight
outerSH = OrbitalRadius `being` outerRadius $ scaleHeight
snowlineSH = OrbitalRadius `being` snowlineRadius $ scaleHeight

mmsnStandardSurfaceDensity :: GramPerCm2 Double
mmsnStandardSurfaceDensity = mkVal 1700


surfaceDensityGas :: Given OrbitalRadius => GramPerCm2 Double
surfaceDensityGas = autoc $ 
  cutoff *|
   mmsnStandardSurfaceDensity |*|
   (fmap (**(-1.5)) $ r |/| (1 *| astronomicalUnit))
  where
    r :: AU Double
    r = the OrbitalRadius
    cutoff =
      sigmoid (val $ (r |-| innerRadius) |/| innerSH) *
      sigmoid (negate $ val $ (r |-| outerRadius) |/| outerSH)

surfaceDensityGasDoc :: MonadAuthoring s w m => m ()
surfaceDensityGasDoc =  
  [rawQ|
\Sigma{}\left(r\right)&=& #{ppValE 1 mmsnStandardSurfaceDensity}
\left(\frac{r}{1\mathrm{au}}\right)^{ -\frac{3}{2}}\mathrm{g/cm^{2}}
   |]



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

temperatureDoc :: MonadAuthoring s w m => m ()
temperatureDoc =  
  [rawQ|
T\left(r\right)&=& 280
\left(\frac{r}{1\mathrm{au}}\right)^{ -\frac{1}{2}}\mathrm{K}
   |]


soundSpeed :: Given OrbitalRadius => CmPerSec Double
soundSpeed = U.sqrt $ autoc cssq
  where
    cssq = (kB |*| temperature) |/| (2.34 *| protonMass)
      `as`  (square (centi meter) |/| square second)

orbitalAngularVelocity :: Given OrbitalRadius => Double :| Hertz
orbitalAngularVelocity =
  U.sqrt $ autoc $ gravitationalConstant |*| solarMass |/| cubic (the OrbitalRadius)

orbitalVelocity :: Given OrbitalRadius => CmPerSec Double
orbitalVelocity =
  U.sqrt $ autoc $ gravitationalConstant |*| solarMass |/| the OrbitalRadius



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