{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Model.Disk.Hayashi where

import           Control.Lens
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
\Omega_K(r) &=& \sqrt{\frac{G M_{\odot}}{r^3}},\\
v_K(r)     &=& \sqrt{\frac{G M_{\odot}}{r}}.
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
  { distanceFromEarth = mkVal 100 
  , inclinationAngle = 0
  , centralStarMass = solarMass
  , gasSurfaceDensity = sdGas
  , temperature = tem
  , bulkMotionSpeed = soundSpeed mmsnModel
  }
  where
    sdGas :: Coord -> GramPerCm2 Double
    sdGas pos = autoc $ 
      cutoff *|
       mmsnStandardSurfaceDensity |*|
       (fmap (**(-1.5)) $ r |/| (1 *| astronomicalUnit))
      where
        cutoff =
          sigmoid (val $ (r |-| innerRadius) |/| innerSH) *
          sigmoid (negate $ val $ (r |-| outerRadius) |/| outerSH)
        r = pos ^. radius 
        
    tem :: Coord -> KelvinUnit Double
    tem pos = 280 *| kelvin |*|
       (fmap (**(-0.5)) $ r |/| (1 *| astronomicalUnit))
      where
        r = pos ^. radius

surfaceDensityGasDoc :: MonadAuthoring s w m => m ()
surfaceDensityGasDoc =  
  [rawQ|
\Sigma{}\left(r\right)&=& #{ppValE 1 mmsnStandardSurfaceDensity}
\left(\frac{r}{1\mathrm{au}}\right)^{ -\frac{3}{2}}\mathrm{g/cm^{2}}
   |]



temperatureDoc :: MonadAuthoring s w m => m ()
temperatureDoc =  
  [rawQ|
T\left(r\right)&=& 280
\left(\frac{r}{1\mathrm{au}}\right)^{ -\frac{1}{2}}\mathrm{K}
   |]


