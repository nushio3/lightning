{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Model.Observation where



import Text.Authoring
import Text.Authoring.TH
import Data.Metrology.Poly
import Data.Metrology.SI.Poly hiding (Length)
import Data.Metrology.Synonyms
import Data.Metrology.Show

import Model.Gas
import Model.Breakdown

sourceDistance :: Length
sourceDistance = (56) % Parsec

arcSecond :: Angle
arcSecond =  (pi/180/3600) % Number

angularResolution :: Angle
angularResolution = 0.025 *| arcSecond

beamSize :: SolidAngle
beamSize = (0.65 *| arcSecond) |*| (0.44 *| arcSecond)

pixelSize :: SolidAngle
pixelSize = angularResolution |^ sTwo


noiseLevelPerbeam :: ChemicalSpecies -> QofU Jansky
noiseLevelPerbeam HCOPlus = redim $ 11.3e-3 % Jansky
noiseLevelPerbeam DCOPlus = redim $ 13.3e-3 % Jansky
noiseLevelPerbeam N2HPlus = redim $ 18.0e-3 % Jansky 
noiseLevelPerbeam c = error $ "noise level undefined for : " ++ show c


psdPerPixel :: ChemicalSpecies -> QofU (Jansky :^ Two)
psdPerPixel chem = redim $ (2 *| noiseLevelPerbeam chem) |^ sTwo |/| (beamSize |/| pixelSize)

psdPerAS2 :: ChemicalSpecies -> QofU (Jansky :^ Two :* Meter :/ Second ) 
psdPerAS2 chem = redim $ (2 *| noiseLevelPerbeam chem) |^ sTwo |*| (beamSize |/| arcSecond|^sTwo) |*| (0.4 % (kilo Meter :/ Second))

measureOfSensitivity ::Int -> ChemicalSpecies -> Maybe BreakdownModel -> Maybe BreakdownModel -> QofU Number
measureOfSensitivity n c a b = 4*|modelNorm n c a b |/|  psdPerPixel c


modelNorm :: Int -> ChemicalSpecies -> Maybe BreakdownModel -> Maybe BreakdownModel -> QofU (Jansky :^ Two) 
modelNorm n c a b = modelNorm' n c a b % (Jansky :^ sTwo) 

modelNorm' :: Int -> ChemicalSpecies -> Maybe BreakdownModel -> Maybe BreakdownModel -> Double

#include "modelNorm25.hs"
#include "modelNorm50.hs"

modelNorm' _ _ _ _ = 0
