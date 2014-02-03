{-# LANGUAGE TemplateHaskell #-}
module Figure.Lime where

import Control.Lens
import System.Process
import Text.Printf

data LimeConfig 
  = LimeConfig 
  { _moldataFileName :: FilePath
  , _imageFileName :: FilePath
  }

defaultLimeConfig = LimeConfig
  { _moldataFileName = "material/lime/hco+@xpol.dat"
  , _imageFileName = "material/lime/tmp.fits"
  }

makeClassy ''LimeConfig

execLime :: LimeConfig -> IO ()
execLime conf = do
  let tmpFn = "material/lime/tmp.c"         
  generateLimeC tmpFn conf         
  system $ printf "yes '' | lime %s" tmpFn         
  return ()

generateLimeC :: FilePath -> LimeConfig -> IO ()
generateLimeC fp conf = writeFile fp $ unlines
  [ printf "char moldata_file_name[] = \"%s\";" $ conf ^. moldataFileName
  , printf "char img_file_name[] = \"%s\";" $ conf ^. imageFileName 
  , printf "double moldata_abundance = %f;" (2.2e-10 :: Double)
  , "#include \"mmsn-model.c\""
  ]
