{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Model.Gas where

import Control.Applicative
import Control.Lens as Lens
import Text.Authoring
import Text.Authoring.TH
import Data.Metrology
import Data.Metrology.AltOperators
import Data.Metrology.Synonyms
import Data.Metrology.SI.GenTypes
import Data.Metrology.SI.Prefixes
import Data.Metrology.SI.Units
import Data.Metrology.Z


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

airMix :: (ChemicalSpecies -> Qu d l Double) -> Qu d l Double
airMix func = 0.78 *. func N2 |+| 0.21 *. func O2 |+| 0.01 *. func Ar

ppdMix :: (ChemicalSpecies -> Qu d l Double) -> Qu d l Double
ppdMix func = 0.9219 *. func H2 |+| 7.7718e-2 *. func He 
              |+| 2.262326e-4 *. func CO |+| 1.3404e-4 *. func O2

atomicNumber :: ChemicalSpecies -> QuOfUL Number MySU
atomicNumber H2 = scalar 2
atomicNumber He = scalar 2
atomicNumber N2 = scalar 14
atomicNumber O2 = scalar 16
atomicNumber CO = scalar 14
atomicNumber Ar = scalar 18
atomicNumber H2O = scalar 10
atomicNumber HCOPlus = scalar 15
atomicNumber DCOPlus = scalar 15
atomicNumber N2HPlus = scalar 15



molecularMass :: ChemicalSpecies -> QuOfUL GramPerMole MySU
molecularMass H2      =   2   % (Gram :/ Mole)
molecularMass He      =   4   % (Gram :/ Mole)
molecularMass N2      =  28   % (Gram :/ Mole)  
molecularMass O2      =  32   % (Gram :/ Mole)
molecularMass CO      =  28   % (Gram :/ Mole)
molecularMass Ar      =  39.9 % (Gram :/ Mole)
molecularMass H2O     =  18   % (Gram :/ Mole)
molecularMass HCOPlus =  29   % (Gram :/ Mole)
molecularMass DCOPlus =  30   % (Gram :/ Mole)
molecularMass N2HPlus =  29   % (Gram :/ Mole)


{- 
Leiden大学のdataabaseがおすすめです。
http://home.strw.leidenuniv.nl/~moldata/
RATRANコードを用いれば、このデータを読み込んで輝線の
輻射輸送計算が簡単に行えます。
http://www.sron.rug.nl/~vdtak/ratran/frames.html
(もう少し大きな分子を扱う場合は、cologne のデータベース(CDMS)
http://www.astro.uni-koeln.de/cdms
-}

rotationalConstant :: ChemicalSpecies -> Frequency MySU Double
rotationalConstant HCOPlus = speedOfLight |*| 1.4875 % (Number :/ Centi :@ Meter)
rotationalConstant DCOPlus = speedOfLight |*| 1.2015  % (Number :/ Centi :@ Meter)
rotationalConstant N2HPlus = speedOfLight |*| 1.55395  % (Number :/ Centi :@ Meter)

rotationalConstant c       = error $ "rotational constant undefined for : " ++ show c


ionizationEnergy :: ChemicalSpecies -> Energy MySU Double
ionizationEnergy = go
  where
    go H2  = 15.43 % ElectronVolt
    go N2  = 15.58 % ElectronVolt
    go CO  = 14.01 % ElectronVolt
    go O2  = 12.07 % ElectronVolt
    go H2O = 12.61 % ElectronVolt
    go Ar  = 15.76 % ElectronVolt
    go He  = 24.58 % ElectronVolt
    go _   = undefined

inelCrossSection :: Int -> ChemicalSpecies -> Area MySU Double    
inelCrossSection 12 N2 = 0.8e-16   % (Centi :@ Meter :^ pTwo)
inelCrossSection 12 O2 = 1.8e-16   % (Centi :@ Meter :^ pTwo)
inelCrossSection 12 Ar = 0         % (Centi :@ Meter :^ pTwo)
inelCrossSection 15 H2 = 1.57e-16  % (Centi :@ Meter :^ pTwo)
inelCrossSection 15 O2 = 1.81e-16  % (Centi :@ Meter :^ pTwo)
inelCrossSection 15 CO = 0.051e-16 % (Centi :@ Meter :^ pTwo)
inelCrossSection _  _  = 0         % (Centi :@ Meter :^ pTwo)

elCrossSection :: Int -> ChemicalSpecies -> Area MySU Double    
elCrossSection 12 N2 = 11.6e-16  % (Centi :@ Meter :^ pTwo)
elCrossSection 12 O2 = 9.00e-16  % (Centi :@ Meter :^ pTwo)
elCrossSection 12 Ar = 17.4e-16  % (Centi :@ Meter :^ pTwo)
elCrossSection 15 H2 = 6.62e-16  % (Centi :@ Meter :^ pTwo)
elCrossSection 15 O2 = 8.89e-16  % (Centi :@ Meter :^ pTwo)
elCrossSection 15 CO = 10.89e-16 % (Centi :@ Meter :^ pTwo)
elCrossSection 15 He = 3.55e-16  % (Centi :@ Meter :^ pTwo)
elCrossSection _  _  = 0         % (Centi :@ Meter :^ pTwo)

airDensity :: Density MySU Double
airDensity = 1.2041 % (kilo Gram :/ Meter :^ pThree)



airNumberDensity :: QuOfUL PerCm3 MySU
airNumberDensity = redim $ airDensity |/| airMix molecularMass |*| avogadroConstant


mfpAir12 :: Length MySU Double
mfpAir12 = redim $ 1 /| airNumberDensity |/| (airMix $ inelCrossSection 12)

mfpAir12E :: Length MySU Double
mfpAir12E = 1 /| airNumberDensity |/| (airMix $ elCrossSection 12)


airDielectricStrengthT :: QuOfUL VoltPerCm MySU
airDielectricStrengthT = redim $ w |/| (mfpAir12 |*| elementaryCharge)
  where w = 12 % ElectronVolt
                    


airDielectricStrengthDP :: QuOfUL VoltPerCm MySU 
airDielectricStrengthDP = redim $ ratio |*| w |/| (0.43 *. elementaryCharge |*| mfpAir12E) 
  where
    w = 12 % ElectronVolt
    ratio = qSqrt $  ratioD         :: QuOfUL Number MySU
    ratioD =  electronMass |/| bigM :: QuOfUL Number MySU
    bigM = (airMix molecularMass |/| avogadroConstant) :: Mass MySU Double


airDielectricStrengthR :: QuOfUL VoltPerCm MySU
airDielectricStrengthR = redim $
  (20.2/(8*pi)) *. (e3 |*| z |*| airNumberDensity)
             |/| (vacuumPermittivity |*| vacuumPermittivity |*| nrg)
  
  where
    nrg :: Energy MySU Double
    nrg = redim $ electronMass |*| speedOfLight |*| speedOfLight
    
    e3 = elementaryCharge |*| elementaryCharge |*| elementaryCharge 
    z = airMix atomicNumber


