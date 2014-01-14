module Model.Disk.Derived where

import Model.Breakdown
import Model.Disk
import Model.Gas
import Model.Disk.Hayashi

lightenedDisk :: BreakdownModel -> Disk -> Disk
lightenedDisk bm disk0 = disk0{lightningAcceleratorField = laf} 
  where
    laf = case bm of
      TownsendBreakdown -> \pos -> ppdDielectricStrengthT pos
      DPBreakdown ->  \pos -> ppdDielectricStrengthDP pos
      RunawayBreakdown ->  \pos -> ppdDielectricStrengthR pos      
