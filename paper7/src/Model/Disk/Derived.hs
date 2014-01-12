module Model.Disk.Derived where

import Control.Lens
import Model.Breakdown
import Model.Disk
import Model.Gas
import Model.Disk.Hayashi

lightenedDisk :: BreakdownModel -> Disk -> Disk
lightenedDisk bm disk0 = disk0 & lightningAcceleratorField .~ laf
  where
    laf = case bm of
      TownsendBreakdown -> \pos -> disk0 >$< pos ^. ppdDielectricStrengthT  
      DPBreakdown       -> \pos -> disk0 >$< pos ^. ppdDielectricStrengthDP 
      RunawayBreakdown  -> \pos -> disk0 >$< pos ^. ppdDielectricStrengthR      
