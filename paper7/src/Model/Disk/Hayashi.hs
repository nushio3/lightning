{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Model.Disk.Hayashi where

import           Control.Lens hiding ((#))
import           Control.Monad.Identity
import           Control.Monad.RWS (tell)
import           Control.Monad.IO.Class
import           Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Text.LaTeX as LTX
import qualified Text.LaTeX.Base.Class as LTX
import qualified Text.LaTeX.Base.Commands as LTX
import qualified Text.LaTeX.Packages.AMSMath as LTX

import           Data.Metrology.Poly
import           Data.Metrology.SI.Units
import           Data.Metrology.Synonyms



import           Model.Disk
import           Model.Values


import           Text.Authoring
import           Text.Authoring.TH

hayashiModelDoc :: MonadAuthoring s w m => m ()
hayashiModelDoc = do
  
  esc "The minimum-mass solar nebula (MMSN) model "
  citet1 "bibcode:1981PThPS..70...35H"
  esc " has been widely used in the studies of the protoplanetary disk, with fruitful results."

  raw "\n\n"
  
  
  environment "eqnarray" $ do
    surfaceDensityGasDoc
    raw ",\\\\"
    temperatureFieldDoc
    raw "."

  [rawQ| The assumption of the hydrostatic equilibrium leads to the vertical distribution of the gas
\begin{eqnarray}
\rho(r,z)&=&\rho_0(r) \exp \left(-\frac{z^2}{2h^2}\right) \nonumber \\
&=&  #{ppValE 2 $ mmsn1au ^. densityGas } 
\left(\frac{r}{1\mathrm{au}}\right)^{ -\frac{3}{2}}
\exp \left(-\frac{z^2}{2h^2}\right) {\mathrm{g ~ cm^{ -3 }}}  ,\\
\mathrm{where} ~~~ 
h(r) &=& \frac{c_s}{\Omega} \nonumber \\
&=&   #{ppValEIn 2 (mmsn1au ^. scaleHeight) AU} 
\left(\frac{r}{1\mathrm{au}}\right)^{ \frac{5}{4}}  {\mathrm{au}} , 
 \\
c_s(r) &=& \sqrt{\frac{k_B T(r)}{\mu m_p}},\\
\Omega_K(r) &=& \sqrt{\frac{G M_{\odot}}{r^3}},\\
v_K(r)     &=& \sqrt{\frac{G M_{\odot}}{r}}.
\end{eqnarray}
   |]

  [rawQ| Therefore the number density of $\mathrm{H_2}$ is
\begin{eqnarray}
n_{\mathrm{H_2}}(r,z)
&=&  #{ppValE 2 $ mmsn1au ^. numberDensityGas } 
\exp \left(-\frac{z^2}{2h^2}\right) {\mathrm{cm^{ -3 }}}  .\\
\end{eqnarray}
   |]


innerRadius, outerRadius, snowlineRadius, innerSH, outerSH, snowlineSH :: Length
innerRadius =     3.5 % AU
outerRadius =     150  % AU
snowlineRadius =  2.7  % AU

innerSH    = mmsnModel >$< equatorAt innerRadius     ^.scaleHeight
outerSH    = mmsnModel >$< equatorAt outerRadius     ^.scaleHeight
snowlineSH = mmsnModel >$< equatorAt snowlineRadius  ^.scaleHeight

mmsnStandardSurfaceDensity :: QofU GramPerCm2
mmsnStandardSurfaceDensity =  640 % (undefined :: GramPerCm2)


mmsnOriginalStandardSurfaceDensity :: QofU GramPerCm2
mmsnOriginalStandardSurfaceDensity =  1700 % (undefined :: GramPerCm2)

mmsn1au :: Environment
mmsn1au = mmsnModel >$< equatorAt1au

mmsnModel :: Disk
mmsnModel 
  = Disk
  { _distanceFromEarth = 100 % AU
  , _inclinationAngle = 0
  , _centralStarMass = solarMass
  , _gasSurfaceDensityField = sdGas
  , _temperatureField = tem
  , _lightningAcceleratorField = const $ zero
  }
  where
    sdGas :: Coord -> QofU GramPerCm2 
    sdGas pos = 
      cutoff *|
       mmsnStandardSurfaceDensity |*
       ( (**(-1.0)) $ (# Number) $ r |/| (1 % AU))
      where
        cutoff :: Double
        cutoff = 
          exp (negate $ (# Number) $ (3 *| r |/| outerRadius) )
--        * (if r |<| innerRadius then 0 else 1) -- there must be no inner-edge cut off here
--        because several power-law constant are origined at 1au.
          
        r = pos ^. radius 
        
    tem :: Coord -> Temperature
    tem pos = 273 % Kelvin  |*
       ( (**(-0.5)) $ (# Number) $ r |/| (1 % AU))
      where
        r = pos ^. radius



mmsnOriginalModel :: Disk
mmsnOriginalModel 
  = Disk
  { _distanceFromEarth = 100 % AU
  , _inclinationAngle = 0
  , _centralStarMass = solarMass
  , _gasSurfaceDensityField = sdGas
  , _temperatureField = tem
  , _lightningAcceleratorField = const $ zero
  }
  where
    sdGas :: Coord -> QofU GramPerCm2 
    sdGas pos = 
      cutoff *|
       mmsnStandardSurfaceDensity |*
       ( (**(-1.5)) $ (# Number) $ r |/| (1 % AU))
      where
        cutoff =
          sigmoid ((# Number) $ (r |-| innerRadius) |/| innerSH) *
          sigmoid (negate $ (# Number) $ (r |-| outerRadius) |/| outerSH)
        r = pos ^. radius 
        
    tem :: Coord -> Temperature
    tem pos = 140 % Kelvin  |*
       ( (**(-0.5)) $ (# Number) $ r |/| (1 % AU))
      where
        r = pos ^. radius

surfaceDensityGasDoc :: MonadAuthoring s w m => m ()
surfaceDensityGasDoc =  
  [rawQ|
\Sigma{}\left(r\right)&=& #{ppValE 1 mmsnStandardSurfaceDensity}
\left(\frac{r}{1\mathrm{au}}\right)^{ -\frac{3}{2}}\mathrm{g/cm^{2}}
   |]



temperatureFieldDoc :: MonadAuthoring s w m => m ()
temperatureFieldDoc =  
  [rawQ|
T\left(r\right)&=& 280
\left(\frac{r}{1\mathrm{au}}\right)^{ -\frac{1}{2}}\mathrm{K}
   |]


