{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Disk.Recent where

import Text.Authoring
import Text.Authoring.TH


aboutLatestDiskModel :: MonadAuthoring s w m => m ()
aboutLatestDiskModel = do
  [rawQ| Recent disk observation suggests that the power-law index 
         of the protoplanetary-disk surface density distribution is
         close to 1 rather than 1.5 
          @{citep ["bibcode:2002ApJ...581..357K",
                   "bibcode:2009ApJ...700.1502A",
                   "bibcode:2010ApJ...723.1241A",
                   "bibcode:2013ASPC..476..387A",
                   "doi:10.1146/annurev-astro-081710-102548"  ]}
         . |]