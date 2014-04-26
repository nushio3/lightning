module Model.Disk.Derived where

import Control.Lens
import Model.Breakdown
import Model.Disk
import Model.Gas
import Model.Disk.Hayashi

import Data.Metrology
import Data.Metrology.Synonyms

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
      let r =  pos ^. radius in
        if (2%AU |<| r && r |<| 200%AU) then x else zero                                