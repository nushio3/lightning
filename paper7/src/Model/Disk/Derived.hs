module Model.Disk.Derived where

import Control.Lens
import Model.Breakdown
import Model.Disk
import Model.Gas
import Model.Disk.Hayashi

import UnitTyped

lightenedDisk :: BreakdownModel -> Disk -> Disk
lightenedDisk bm disk0 = disk0 & lightningAcceleratorField .~ laf
  where
    laf = case bm of
      TownsendBreakdown -> \pos -> disk0 >$< pos ^. ppdDielectricStrengthT  
      DPBreakdown       -> \pos -> disk0 >$< pos ^. ppdDielectricStrengthDP 
      RunawayBreakdown  -> \pos -> disk0 >$< pos ^. ppdDielectricStrengthR      
      
                               
lightenedDiskEx :: BreakdownModel -> Disk -> Disk
lightenedDiskEx bm disk0 = disk0 & lightningAcceleratorField .~ laf
  where
    laf = case bm of
      TownsendBreakdown -> \pos -> boundBy pos $ disk0 >$< pos ^. ppdDielectricStrengthT  
      DPBreakdown       -> \pos -> boundBy pos $ disk0 >$< pos ^. ppdDielectricStrengthDP 
      RunawayBreakdown  -> \pos -> boundBy pos $ disk0 >$< pos ^. ppdDielectricStrengthR      
      
    boundBy pos x = 
      let r = val $ pos ^. radius in
        if r > 100 then x else mkVal 0                                