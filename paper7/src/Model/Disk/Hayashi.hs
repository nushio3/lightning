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


import           Model.Disk
import           Model.Values


import           Text.Authoring
import           Text.Authoring.TH

hayashiModelDoc :: MonadAuthoring s w m => m ()
hayashiModelDoc = do
  citet1 "bibcode:1981PThPS..70...35H"
  
  esc " has proposed the following minimum-mass solar nebula (MMSN) model of the protoplanetary disk."
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

innerSH    = scaleHeight mmsnModel $ equatorAt innerRadius     
outerSH    = scaleHeight mmsnModel $ equatorAt outerRadius     
snowlineSH = scaleHeight mmsnModel $ equatorAt snowlineRadius  

mmsnStandardSurfaceDensity :: GramPerCm2 Double
mmsnStandardSurfaceDensity = mkVal 1700

mmsnModel :: Disk
mmsnModel 
  = Disk
  { inclinationAngle = 0
  , centralStarMass = solarMass
  , gasSurfaceDensity = sdGas
  , temperature = tem
  }
  where
    sdGas :: Coord -> GramPerCm2 Double
    sdGas (Coord r _) = autoc $ 
      cutoff *|
       mmsnStandardSurfaceDensity |*|
       (fmap (**(-1.5)) $ r |/| (1 *| astronomicalUnit))
      where
        cutoff =
          sigmoid (val $ (r |-| innerRadius) |/| innerSH) *
          sigmoid (negate $ val $ (r |-| outerRadius) |/| outerSH)

    tem :: Coord -> KelvinUnit Double
    tem (Coord r _) = 280 *| kelvin |*|
       (fmap (**(-0.5)) $ r |/| (1 *| astronomicalUnit))


surfaceDensityGasDoc :: MonadAuthoring s w m => m ()
surfaceDensityGasDoc =  
  [rawQ|
\Sigma{}\left(r\right)&=& #{ppValE 1 mmsnStandardSurfaceDensity}
\left(\frac{r}{1\mathrm{au}}\right)^{ -\frac{3}{2}}\mathrm{g/cm^{2}}
   |]



densityGas :: Disk -> Coord -> GramPerCm3 Double
densityGas disk pos = autoc $
  factor *| (gasSurfaceDensity disk pos) |/| h
  where
    (Coord _ z) = pos
    factor = (2*pi)**(-1/2)
           * (exp(negate $ val (square z |/| (2 *| square h))))
    h = scaleHeight disk pos



temperatureDoc :: MonadAuthoring s w m => m ()
temperatureDoc =  
  [rawQ|
T\left(r\right)&=& 280
\left(\frac{r}{1\mathrm{au}}\right)^{ -\frac{1}{2}}\mathrm{K}
   |]


soundSpeed :: Disk -> Coord -> CmPerSec Double
soundSpeed disk pos = U.sqrt $ autoc cssq
  where
    cssq :: Cm2PerSec2 Double
    cssq = autoc $ (kB |*| (temperature disk pos)) 
               |/| (2.34 *| protonMass)

orbitalAngularVelocity :: Disk -> Coord -> Double :| Hertz
orbitalAngularVelocity disk (Coord r _) =
  U.sqrt $ autoc $ gravitationalConstant |*| mSun |/| cubic r
  where
    mSun = centralStarMass mSun
    
orbitalVelocity :: Disk -> Coord -> CmPerSec Double
orbitalVelocity  disk (Coord r _) =
  U.sqrt $ autoc $ gravitationalConstant |*| mSun |/| r
  where
    mSun = centralStarMass mSun


scaleHeight :: Disk -> Coord -> AU Double
scaleHeight disk = f
  where 
    f pos = 
      autoc $ (soundSpeed disk pos) 
          |/| (orbitalAngularVelocity disk pos)

sigmoid :: Double -> Double
sigmoid x = 1/(1+exp (negate x))

gaussian :: Double -> Double -> Double -> Double
gaussian mu sigma x = 1/sqrt(2*pi* sigma^2) * exp (negate $ (/2) $ ((x-mu)/sigma)^2)

