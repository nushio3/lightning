{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Disk.Recent where

import Text.Authoring
import Text.Authoring.TH

import           Data.Metrology.Poly
import           Data.Metrology.SI.Units
import           Data.Metrology.Synonyms


calvetStandardSurfaceDensity :: QofU GramPerCm2
calvetStandardSurfaceDensity =  640 % (undefined :: GramPerCm2)


aboutLatestDiskModel :: MonadAuthoring s w m => m ()
aboutLatestDiskModel = do
  [rawQ| 
    Recent observations have proposed sophisticated models of disks and also have reported qualitative values
    for the inner and outer edge radius of the disks. 
    @{citep ["bibcode:2002ApJ...581..357K",
             "bibcode:2009ApJ...700.1502A",
             "bibcode:2010ApJ...723.1241A",
             "doi:10.1146/annurev-astro-081710-102548"]}.
    However, such observational values  for
    geometry and mass of
    specific objects still contain uncertainty factors of 2-3, and are subjects of debate.
    See e.g. 
    @{citet ["arxiv:1402.6597"]}.

    Some of the features common to the recent models are that the power-law index 
    of the surface density distribution is
    close to 1 rather than 1.5, and that there are exponential cut-off at the outer
    edge of the disk
    Therefore, we use simple model proposed by
    @{citet ["doi:10.1093/pasj/65.6.123"]} that captures these common features,
    and adopt the values of TW Hya reported by
    @{citet ["bibcode:2002ApJ...568.1008C"]}. Our disk model is as follows:
    
\begin{eqnarray}
\Sigma{}\left(r\right)&=& #{ppValE 1 calvetStandardSurfaceDensity}
\left(\frac{r}{1\mathrm{au}}\right)^{ -1} 
\exp\left(-\frac{3r}{r_{\rm out}}\right)
\mathrm{g/cm^{2}} 
\hspace{1cm} {\rm for}~ ~  r>r_{\rm in}, \nonumber
\\
\Sigma{}\left(r\right)&=&0\hspace{1cm} {\rm otherwise} ,
\\
T\left(r\right)&=& 273
\left(\frac{r}{1\mathrm{au}}\right)^{ -\frac{1}{2}}\mathrm{K} .
\end{eqnarray} 

Here, $r_{\rm out} = 150 {\rm au}$ is the outer radius of our model disk.
We also introduce an inner cutoff at
 $r_{\rm in} = 3.5 {\rm au}$.
          |]


--  ,                   