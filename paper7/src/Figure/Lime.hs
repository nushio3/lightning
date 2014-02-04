{-# LANGUAGE TemplateHaskell #-}
module Figure.Lime where

import Control.Lens
import System.Process
import Text.Printf

data LimeConfig 
  = LimeConfig 
  { _moldataFileName :: FilePath
  , _molAbundance :: Double
  , _imageFileName :: FilePath
  , _pvDataFileName :: FilePath
  , _pvEpsFileName :: FilePath
  , _velocityChannelNumber :: Int
  , _velocityResolution :: Double
  , _lightningVelocity :: Double
  , _lightningInnerRadius :: Double
  , _lightningOuterRadius :: Double 
  }

defaultLimeConfig = LimeConfig
  { _moldataFileName = "material/lime/hco+@xpol.dat"
  , _molAbundance = 2.2e-8
  , _imageFileName = "material/lime/tmp.fits"
  , _pvDataFileName = "material/lime/tmp-pv.txt"
  , _pvEpsFileName = "material/lime/tmp-pv.eps"
  , _velocityChannelNumber = 120
  , _velocityResolution = 50
  , _lightningVelocity = 490
  , _lightningInnerRadius = 100
  , _lightningOuterRadius = 200
  }

makeClassy ''LimeConfig

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