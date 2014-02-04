{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Figure.Lime where

import Control.Lens
import Data.Char
import System.Process
import Text.Printf
import UnitTyped (val,autoc)
import UnitTyped.Synonyms (MPerSec)
import Model.Breakdown
import Model.Gas
import Model.RadiativeTransfer
import Model.Disk
import Model.Disk.Derived
import Model.Disk.Hayashi

data LimeConfig 
  = LimeConfig 
  { _targetMolecule :: ChemicalSpecies
  , _targetLightningModel :: Maybe BreakdownModel
  , _fileNameBody :: String
  , _velocityChannelNumber :: Int
  , _velocityResolution :: Double
  , _lightningInnerRadius :: Double
  , _lightningOuterRadius :: Double 
  }
makeClassy ''LimeConfig

defaultLimeConfig = LimeConfig
  { _targetMolecule = HCOPlus
  , _targetLightningModel = Just TownsendBreakdown
  , _fileNameBody = "LgRg"
  , _velocityChannelNumber = 201
  , _velocityResolution = 200
  , _lightningInnerRadius = 50
  , _lightningOuterRadius = 100
  }

moldataFileName :: Getter LimeConfig FilePath
moldataFileName = to go
  where
    go conf = case conf^.targetMolecule of
      HCOPlus -> "material/lime/hco+@xpol.dat"
      DCOPlus -> "material/lime/dco+@xpol.dat"
      N2HPlus -> "material/lime/n2h+@xpol.dat"
      x       -> error $ "unknown species: " ++ show x

molAbundance :: Getter LimeConfig Double
molAbundance = to go
  where 
    go conf = val $ fractionalAbundance100au $ conf^.targetMolecule


mkFileNameGetter :: String -> Getter LimeConfig String
mkFileNameGetter ext = to go
  where
    go conf = printf "material/lime-output/%s-%s-%s-R%d_%d-V%fx%d%s"
      (conf ^. fileNameBody) 
      (show $ conf ^. targetMolecule)
      (filter isUpper $ show $ conf ^. targetLightningModel)
      (round $ conf ^. lightningInnerRadius :: Int)
      (round $ conf ^. lightningOuterRadius :: Int)
      (conf ^. velocityResolution)
      (conf ^. velocityChannelNumber)
      ext

imageFileName  = mkFileNameGetter ".fits"
pvDataFileName = mkFileNameGetter "-pv.txt"
pvEpsFileName  = mkFileNameGetter "-pv.eps"

lightningVelocity :: Getter LimeConfig Double
lightningVelocity = to go
  where 
    go :: LimeConfig -> Double
    go conf = val
      (autoc $ fieldToVelocity (disk1 conf) (conf ^. targetMolecule) 
       :: MPerSec Double)  
    disk1 conf = case (conf^.targetLightningModel) of
      Nothing -> mmsn1au
      Just bm -> mmsn1au & disk %~ lightenedDisk bm


execLime :: LimeConfig -> IO ()
execLime conf = do
  let tmpFn = "material/lime/tmp.c"         
      tmpPyFn = "material/lime/tmp.py"         
      tmpGnuplotFn = "material/lime/tmp.gnuplot"         
  generateLimeC tmpFn conf         
  system $ printf "yes '' | lime %s" tmpFn         
  generatePy tmpPyFn conf         
  system $ printf "python %s %s" tmpPyFn tmpFn
  generateGnuplot tmpGnuplotFn conf         
  system $ printf "gnuplot %s" tmpGnuplotFn
  return ()

generateLimeC :: FilePath -> LimeConfig -> IO ()
generateLimeC fp conf = writeFile fp $ unlines
  [ printf "char MoldataFileName[] = \"%s\";" $ conf ^. moldataFileName
  , printf "char ImageFileName[] = \"%s\";" $ conf ^. imageFileName 
  , printf "double MolAbundance = %f;" $ conf ^. molAbundance
  , printf "double VelocityResolution = %f;" $ conf ^. velocityResolution
  , printf "int VelocityChannelNumber = %d;" $ conf ^. velocityChannelNumber
  , printf "double LightningVelocity = %f;" $ conf ^. lightningVelocity
  , printf "double LightningInnerRadius = %f;" $ conf ^. lightningInnerRadius
  , printf "double LightningOuterRadius = %f;" $ conf ^. lightningOuterRadius
  , "#include \"mmsn-model.c\""
  ]

generatePy :: FilePath -> LimeConfig -> IO ()
generatePy fp conf = writeFile fp $ unlines
  [ "#!/usr/bin/env python"
  , "import pyfits"
  , printf "hdulist = pyfits.open('%s')" $ conf^.imageFileName
  , "img = hdulist[0].data"
  , printf "nv = %d" $ conf ^. velocityChannelNumber
  , printf "fp = open('%s','w')" $ conf^. pvDataFileName
  , "for i in range(nv):"
  , printf
    "    vL = (-%d * 0.5 + i) * %f" 
         (conf ^. velocityChannelNumber) (conf ^. velocityResolution)
  , printf
    "    vR = (-%d * 0.5 + i + 1) * %f" 
         (conf ^. velocityChannelNumber) (conf ^. velocityResolution)
  , "    jy = img[i].sum()"
  , "    print >> fp, vL, jy"
  , "    print >> fp, vR, jy"
  ]

generateGnuplot :: FilePath -> LimeConfig -> IO ()
generateGnuplot fp conf = writeFile fp $ unlines
  [ "set term postscript enhanced color solid 30"
  , printf "set grid"
  , printf "set xlabel 'velocity (km/s)'"
  , printf "set ylabel 'spectral flux density (Jy)'"
  , printf "set log y"  
  , printf "set out '%s'" $ conf^. pvEpsFileName
  , printf "plot '%s' u ($1/1e3):($2) w l lw 2 t ''" $ conf^.pvDataFileName
  ]