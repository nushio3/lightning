{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Paper.Slides where

import           Control.Lens ((^.))
import           Control.Monad.RWS
import           Control.Monad.State.Strict (modify)
import           Data.Default (def)
import           Data.List (break)
import           Data.Monoid ((<>))
import           Data.Functor.Identity
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import           Text.Authoring
import           Text.Authoring.TH
import           Text.LaTeX.Base.Syntax (LaTeX(..),TeXArg(..))
import           Text.LaTeX.Base.Writer (LaTeXT(..), execLaTeXT)
import qualified Text.LaTeX as LTX

import           Model.Gas

import           Data.Metrology
import           Data.Metrology.Synonyms
import           Data.Metrology.SI.Units
import           Data.Metrology.SI.Prefixes
import           Data.Metrology.LaTeX



writePaper :: FilePath -> FilePath -> FilePath -> IO ()
writePaper srcFn outFn bibFn = do
  srcStr <- Text.readFile srcFn
  print $ (srcFn, outFn)

  (bodyText,bibText) <- genBodyText

  let
    (<?) = Text.isInfixOf
    rep str
      | "Insert document body here" <? str = bodyText
      | otherwise                          = str

  Text.writeFile outFn $
    Text.unlines $
    map rep $ Text.lines srcStr

  Text.writeFile bibFn bibText


genBodyText :: IO (Text.Text, Text.Text)
genBodyText = do
  (bibText, _ , bodyDoc) <- runAuthoringT $ do
    withDatabaseFile "material/citation.db" sectionSlides
    txt <- bibliographyContent
    return txt



  return (LTX.render bodyDoc, bibText)

sectionSlides :: MonadAuthoring s w m => m ()
sectionSlides = do
  let cm2 = centi Meter :^ pTwo
      cm3 = centi Meter :^ pThree
      cmM3 = centi Meter :^ pMThree      
      kvcm = kilo Volt :/ centi Meter
  
  command1 "section" $ raw "Method"
  environment "frame" $ do
    environment "tabular" $ do
          
      raw "{c|ccc}"
      [rawQ| \hline \hline |]
      [rawQ|  & $\rm N_2$ & $\rm O_2$ & $\rm Ar$ \\ |]
      [rawQ| \hline |]
      [rawQ| Volume fraction & 78\% & 21\% & 1\% \\|]
      [rawQ| $\Delta W$ 
             & $#{ppF ElectronVolt "%.2f" $ ionizationEnergy N2}$   
             & $#{ppF ElectronVolt "%.2f" $ ionizationEnergy O2}$   
             & $#{ppF ElectronVolt "%.2f" $ ionizationEnergy Ar}$   
             \\|]      
      [rawQ| $\sigma_{\rm inel}$ 
             & $#{ppFP cm2 "%.1f" (-16) $ inelCrossSection 12 N2}$   
             & $#{ppFP cm2 "%.1f" (-16) $ inelCrossSection 12 O2}$   
             & $#{ppFP cm2 "%.1f" (-16) $ inelCrossSection 12 Ar}$   
             \\|]     
      [rawQ| \hline |]
    environment "itemize" $ do
      [rawQ| 
        \item
          Air number density at NTP is $#{ppE cmM3 3 airNumberDensity}$ 
        \item
          The mean inelastic cross section of air at 12eV is 
          $#{ppE cm2 1 $ airMix $ inelCrossSection 12}$.
        \item
          $l_{\rm mfp} = (n_n \sigma_{inel})^{ -1 } = #{ppE (centi Meter) 1 mfpAir12}$ 
        \item
         This gives 
         $E_{\rm crit} = #{ppF kvcm "%.2f" $ airDielectricStrengthT}$,
         which is in good agreement with lab experiments. |]


  environment "frame" $ do
    environment "itemize" $ do
      raw "\\item meter \\pause"            
      raw "\\item kg \\pause"            
      raw "\\item second \\pause"            
      
