{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Gas where

import Control.Applicative
import Control.Lens as Lens
import Text.Authoring
import Text.Authoring.TH
import Data.Metrology
import Data.Metrology.Synonyms

import Model.Values

data ChemicalSpecies
  = H2
  | N2
  | O2
  | CO
  | H2O
  | Ar
  | He
  | HCOPlus
  | DCOPlus
  | N2HPlus
  deriving (Eq, Show)    

airMix :: Convertible' a b => (ChemicalSpecies -> Value a b Double) -> Value a b Double
airMix func = 0.78 *. func N2 |+| 0.21 *. func O2 |+| 0.01 *. func Ar

ppdMix :: Convertible' a b => (ChemicalSpecies -> Value a b Double) -> Value a b Double
ppdMix func = 0.9219 *. func H2 |+| 7.7718e-2 *. func He 
              |+| 2.262326e-4 *. func CO |+| 1.3404e-4 *. func O2

atomicNumber :: ChemicalSpecies -> NoDimension Double 
atomicNumber H2 = mkVal 2
atomicNumber He = mkVal 2
atomicNumber N2 = mkVal 14
atomicNumber O2 = mkVal 16
atomicNumber CO = mkVal 14
atomicNumber Ar = mkVal 18
atomicNumber H2O = mkVal 10
atomicNumber HCOPlus = mkVal 15
atomicNumber DCOPlus = mkVal 15
atomicNumber N2HPlus = mkVal 15



molecularMass :: ChemicalSpecies -> GramPerMole Double 
molecularMass H2 = mkVal 2
molecularMass He = mkVal 4
molecularMass N2 = mkVal 28
molecularMass O2 = mkVal 32
molecularMass CO = mkVal 28
molecularMass Ar = mkVal 39.9
molecularMass H2O = mkVal 18
molecularMass HCOPlus = mkVal 29
molecularMass DCOPlus = mkVal 30
molecularMass N2HPlus = mkVal 29

{- 
Leiden大学のdataabaseがおすすめです。
http://home.strw.leidenuniv.nl/~moldata/
RATRANコードを用いれば、このデータを読み込んで輝線の
輻射輸送計算が簡単に行えます。
http://www.sron.rug.nl/~vdtak/ratran/frames.html
(もう少し大きな分子を扱う場合は、cologne のデータベース(CDMS)
http://www.astro.uni-koeln.de/cdms
-}

rotationalConstant :: ChemicalSpecies -> PerSecond Double
rotationalConstant HCOPlus = autoc $ speedOfLight .* (mkVal 1.4875 :: PerCm Double)
rotationalConstant DCOPlus = autoc $ speedOfLight .* (mkVal 1.2015 :: PerCm Double)
rotationalConstant N2HPlus = autoc $ speedOfLight .* (mkVal 1.55395 :: PerCm Double)

rotationalConstant c       = error $ "rotational constant undefined for : " ++ show c


ionizationEnergy :: ChemicalSpecies -> ElectronVolt Double
ionizationEnergy = go
  where
    go H2  = mkVal 15.43
    go N2  = mkVal 15.58
    go CO  = mkVal 14.01
    go O2  = mkVal 12.07
    go H2O = mkVal 12.61
    go Ar  = mkVal 15.76
    go He  = mkVal 24.58
    go _   = undefined

inelCrossSection :: Int -> ChemicalSpecies -> Cm2 Double    
inelCrossSection 12 N2 = mkVal 0.8e-16
inelCrossSection 12 O2 = mkVal 1.8e-16
inelCrossSection 12 Ar = mkVal 0
inelCrossSection 15 H2 = mkVal 1.57e-16
inelCrossSection 15 O2 = mkVal 1.81e-16
inelCrossSection 15 CO = mkVal 0.051e-16
inelCrossSection _  _  = mkVal 0

elCrossSection :: Int -> ChemicalSpecies -> Cm2 Double    
elCrossSection 12 N2 = mkVal 11.6e-16
elCrossSection 12 O2 = mkVal 9.00e-16
elCrossSection 12 Ar = mkVal 17.4e-16
elCrossSection 15 H2 = mkVal 6.62e-16
elCrossSection 15 O2 = mkVal 8.89e-16
elCrossSection 15 CO = mkVal 10.89e-16
elCrossSection 15 He = mkVal 3.55e-16
elCrossSection _  _  = mkVal 0

airDensity :: GramPerCm3 Double
airDensity = mkVal 1.2041e-3

airNumberDensity :: PerCm3 Double
airNumberDensity = autoc $ airDensity ./ airMix molecularMass


mfpAir12 :: Cm Double
mfpAir12 = autoc $ 1 /| airNumberDensity ./ (airMix $ inelCrossSection 12)

mfpAir12E :: Cm Double
mfpAir12E = autoc $ 1 /| airNumberDensity ./ (airMix $ elCrossSection 12)


airDielectricStrengthT :: VoltPerCm Double
airDielectricStrengthT = autoc $ w ./ (mfpAir12 .* elementaryCharge)
  where w = mkVal 12 :: ElectronVolt Double
                        


airDielectricStrengthDP :: VoltPerCm Double
airDielectricStrengthDP = autoc $ ratio *. w ./ (0.43 *. elementaryCharge .* mfpAir12E) 
  where
    w = mkVal 12                           :: ElectronVolt Double
    ratio = sqrt $ val ratioD              :: Double
    ratioD = autoc $ electronMass ./ bigM :: NoDimension Double
    bigM = autoc $ airMix molecularMass    :: GramUnit Double


airDielectricStrengthR :: VoltPerCm Double
airDielectricStrengthR = autoc $ 
  (20.2/(8*pi)) *. (e3 .* z .* airNumberDensity)
             ./ (vacuumPermittivity .* vacuumPermittivity .* nrg)
  
  where
    nrg :: JouleUnit Double
    nrg = autoc $ electronMass .* speedOfLight .* speedOfLight
    
    e3 = elementaryCharge .* elementaryCharge .* elementaryCharge 
    z = airMix atomicNumber
    n = airNumberDensity



