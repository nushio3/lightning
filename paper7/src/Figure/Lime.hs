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

makeClassy ''LimeConfig

execLime :: LimeConfig -> IO ()
execLime conf = do
  let tmpFn = "material/lime/tmp.c"         
  writeLimeConfig tmpFn conf         
  system $ printf "lime %s" tmpFn         
  return ()

writeLimeConfig :: FilePath -> LimeConfig -> IO ()
writeLimeConfig fp conf = writeFile fp $ unlines
  [ printf "char moldata_file_name[] = \"%s\";" $ conf ^. moldataFileName
  , printf "char img_file_name[] = \"%s\";" $ conf ^. imageFileName 
  , printf "moldata_abundance = %f;" (2.2e-10 :: Double)
  , "#include \"mmsn-mode.c\""
  ]
