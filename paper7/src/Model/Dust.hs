{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Model.Dust where

import           Data.Monoid ((<>))
import qualified Text.LaTeX as LTX
import qualified Text.LaTeX.Base.Class as LTX
import qualified Text.LaTeX.Packages.AMSMath as LTX
import           Data.Metrology.Poly
import           Data.Metrology.Synonyms


dustSurfaceCharge :: QofU  CoulombPerCm2 
dustSurfaceCharge = 1e-10 % (undefined :: CoulombPerCm2)
