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
import           Data.Metrology.Synonyms


dustSurfaceCharge :: CoulombPerCm2 Double
dustSurfaceCharge = mkVal 1e-10
