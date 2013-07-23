{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Model.Disk.Hayashi where

import           Control.Monad.Author
import           Data.Monoid ((<>))
import qualified Text.LaTeX as LTX
import qualified Text.LaTeX.Base.Class as LTX
import qualified Text.LaTeX.Packages.AMSMath as LTX
import qualified Text.LaTeX.Utils as LTX
import           UnitTyped
import           UnitTyped.SI
import           UnitTyped.SI.Constants
import           UnitTyped.SI.Meta
import           UnitTyped.SI.Derived.Length
import           UnitTyped.SI.Derived.Time
import           UnitTyped.Synonyms
import qualified UnitTyped.NoPrelude as U

hayashiModelDoc :: Monad m =>  AuthorT m ()
hayashiModelDoc = do
  "Hayashi model is as follows."
  LTX.eqnarray $ do
    surfaceDensityDoc

innerRadius, outerRadius, snowlineRadius :: Floating a => a :| AstronomicalUnit
innerRadius = 0.35  *| astronomicalUnit
outerRadius = 2.7   *| astronomicalUnit
snowlineRadius = 36 *| astronomicalUnit


surfaceDensity :: Floating a => a :| AstronomicalUnit -> GPerCM2 a

surfaceDensity r =
  cutoff *|
   (1700 *| gram |/| square (centi meter)) |*|
   (fmap (**(-1.5)) $ r |/| (1 *| astronomicalUnit))
  where
    cutoff =
      sigmoid (val $ (r |-| innerRadius) |/| scaleHeight r) *
      sigmoid (val $ (r |-| outerRadius) |/| scaleHeight r)

surfaceDensityDoc :: Monad m => AuthorT m ()
surfaceDensityDoc = do
  LTX.sigmau <> LTX.autoParens "r" LTX.& "=" LTX.& ""
  1.7e3 `LTX.times` (LTX.autoParens ("r" / ("1" <> LTX.mathrm "au")) ** (negate $ 3/2))
  LTX.mathrm ("g/cm" **2)

temperatureField :: Floating a => a :| AstronomicalUnit -> a :| Kelvin
temperatureField r = 280 *| kelvin |*|
   (fmap (**(-0.5)) $ r |/| (1 *| astronomicalUnit))

soundSpeedField :: Floating a => a :| AstronomicalUnit ->
 Value '[ '(Length, POne),  '(Time, NOne)] '[ '(Centi Meter, POne), '(Second, NOne) ] a
soundSpeedField r = U.sqrt $ autoc cssq
  where
    cssq = (kB |*| temperatureField r) |/| (2.34 *| m_p)
      `as`  (square (centi meter) |/| square second)

orbitalAngularVelocity :: Floating a => a :| AstronomicalUnit -> a :| Hertz
orbitalAngularVelocity r =
  U.sqrt $ autoc $ g |*| m_p |/| cubic r


scaleHeight :: Floating a => a :| AstronomicalUnit -> a :| AstronomicalUnit
scaleHeight r = autoc $ soundSpeedField r |/| orbitalAngularVelocity r

sigmoid :: Floating a => a -> a
sigmoid x = 1/(1+exp (negate x))