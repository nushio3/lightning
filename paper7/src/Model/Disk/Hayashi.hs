{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Model.Disk.Hayashi where

import UnitTyped 
import UnitTyped.SI 
import UnitTyped.SI.Constants
import UnitTyped.SI.Meta
import UnitTyped.SI.Derived.Length 
import UnitTyped.SI.Derived.Time

import qualified UnitTyped.NoPrelude as U 

innerRadius, outerRadius, snowlineRadius :: Fractional a => a :| AstronomicalUnit
innerRadius = 0.35*| astronomicalUnit
outerRadius = 2.7*| astronomicalUnit
snowlineRadius = 36 *| astronomicalUnit


surfaceDensity :: Floating a => a :| AstronomicalUnit -> 
 Value '[ '(Mass, POne),  '(Length, NTwo)] '[ '(Gram, POne), '(Centi Meter, NTwo) ] a

surfaceDensity r = 
  cutoff *|
   (1700 *| gram |/| square (centi meter)) |*| 
   (fmap (**(-1.5)) $ r |/| (1 *| astronomicalUnit))
  where
    cutoff = 
      sigmoid (val $ (r |-| innerRadius) |/| scaleHeight r) *
      sigmoid (val $ (r |-| outerRadius) |/| scaleHeight r) 

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