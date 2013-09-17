{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Model.Dust where

import           Data.Reflection.Typed
import           Data.Monoid ((<>))
import           Model.Concepts
import qualified Text.LaTeX as LTX
import qualified Text.LaTeX.Base.Class as LTX
import qualified Text.LaTeX.Packages.AMSMath as LTX
import qualified Text.LaTeX.Utils as LTX
import           UnitTyped
import           UnitTyped.SI
import           UnitTyped.SI.Constants
import           UnitTyped.SI.Meta
import           UnitTyped.SI.Derived.Length
import           UnitTyped.SI.Derived.Time
import           UnitTyped.Synonyms
import qualified UnitTyped.NoPrelude as U

import           Text.LaTeX.Author

dustSurfaceCharge :: CoulombPerCm2 Double
dustSurfaceCharge = mkVal 1e-10
