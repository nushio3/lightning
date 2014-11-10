{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Paper where

import           Control.Lens ((^.))
import           Control.Monad.RWS
import           Control.Monad.State.Strict (modify)
import           Data.Default (def)
import           Data.List (break)
import           Data.Monoid ((<>))
import           Data.Functor.Identity
import qualified Data.Map as Map
import           Data.Metrology.Poly
import           Data.Metrology.Synonyms
import qualified Data.Set as Set
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import           Text.Authoring
import           Text.Authoring.TH
import           Text.LaTeX.Base.Syntax (LaTeX(..),TeXArg(..))
import           Text.LaTeX.Base.Writer (LaTeXT(..), execLaTeXT)
import qualified Text.LaTeX as LTX
import           Text.Printf

import           Model.Observation(sourceDistance)
import           Paper.SectionModel (sectionModel)
import           Paper.SectionMatchedFilter (sectionMF)
import           Paper.SectionObservation (sectionObservation)
import           Paper.SectionCrossSection (sectionCrossSection)
import           Paper.SectionAcknowledgement (sectionAcknowledgement)


writePaper :: FilePath -> FilePath -> FilePath -> IO ()
writePaper srcFn outFn bibFn = do
  srcStr <- Text.readFile srcFn
  print $ (srcFn, outFn)

  (bodyText,bibText) <- genBodyText

  let
    (<?) = Text.isInfixOf
    rep str
      | "Insert abstract here" <? str      = abstractText
      | "Insert document body here" <? str = bodyText
      | otherwise                          = str

  Text.writeFile outFn $
    Text.unlines $
    map rep $ Text.lines srcStr

  Text.writeFile bibFn bibText

abstractText :: Text.Text
abstractText = LTX.render abstract

abstract :: LaTeX
abstract = TeXRaw $ Text.pack $ printf
  "In this paper, we study the observational methods for detecting and distinguishing lightning models in protoplanetary disks. We do so by observing the dielectric strength of the lightning matrix gas (LMG), the region of the disk where the electric field is strong enough for lightning. We utilize the fact that multiple positive ion species are accelerated to their characteristic terminal velocities due to the electric field. In this paper, we present three distinct discharge models. We simulate the position-velocity diagrams and the integrated emission maps for the models. We calculate the measure of sensitivity values for detection of the models, and distinguishing between the models. At the distance of TW-Hya (%.0fpc), LMG that occupies $2\\pi$ in azimuth and $25 \\mathrm{au}<r<50 \\mathrm{au}$ is  $125\\sigma$- to  $1000\\sigma$-detectable. The upper limits of the radii of $5\\sigma$-detectable LMG clumps are between 3.0 au and 18.1 au, depending on the models." (sourceDistance # Parsec)

   




genBodyText :: IO (Text.Text, Text.Text)
genBodyText = do
  let paper = do
        sectionIntro
        sectionModel
        sectionObservation
        sectionMF
        sectionConclusion
        sectionAcknowledgement
        
        [rawQ| \appendix |]

        sectionCrossSection         

  
  (bibText, _ , bodyDoc) <- runAuthoringT $ do
    withDatabaseFile "material/citation.db" paper
    txt <- bibliographyContent
    return txt



  return (LTX.render bodyDoc, bibText)

sectionIntro :: MonadAuthoring s w m => m ()
sectionIntro = do
  command1 "section" $ raw "INTRODUCTION"
  
  [rawQ| 
Lightning in protoplanetary disks is one of the important topic in protoplanetary disk physics, because it is one of the elementary electromagnetic processes in the disks, it is one of the observational clue to measure the electromagnetic states of the disk, and it is one of the candidate mechanism for chondrule heating. Meanwhile, the size of the observation data archive has been  drastically increasing, opening access to the observational results from the most advanced telescopes. However, observation methods of the protoplanetary lightning using the advanced telescopes have not been seriously studied.

There has been a controversial debate on the existence and the mechanism of
protoplanetary disk lightning. 
@{citet ["doi:10.1006/icar.1997.5846"]} argued that plasma conductivity is too large
for the lightning to take place.
@{citet ["bibcode:1998A&A...331..121P"]}
argued that unknown, efficient grain-grain charging process
is required to produce lightning.
Despite of these barriers, mechanisms that lead to lightning are proposed:
dust-dust collisional charging @{citep ["bibcode:2000Icar..143...87D","bibcode:2010MNRAS.401.2641M"]};
mutual positive feedback of thermal ionization and Joule heating @{citep ["bibcode:2012ApJ...761...58H","bibcode:2013ApJ...767L...2M"]}; electric field generated by magnetorotational instability (MRI)
 @{citep ["doi:10.1086/432796","bibcode:2012ApJ...760...56M"]}.
Chondrules included in meteorites carry unmodified materials from the protoplanetary disks that formed 
our Solar System that exhibit evidences of lightning
@{citep ["bibcode:2000M&PS...35..537W"]}.

Meanwhile,  these twenty years saw progress  in the understanding of the lightning ignition mechanism.
Attempts have been made to explain the mechanism that causes discharge at the point
well below the nominal dielectric strength of air
@{citep ["doi:10.1029/JC076i024p05799", "doi:10.1063/1.1656844"]}. As a result 
new model of lightning such as Runaway breakdown
@{citep ["doi:10.1016/0375-9601(92)90348-P", "doi:10.1070/PU2001v044n11ABEH000939"]}
has been proposed. We take such new models into account and propose a new observation method for
the detection of the
protoplanetary disk lightning. 
Observation of the disk lightning will contribute to the
understanding of the electromagnetic process in the protoplanetary disk,
and the source of chondrule heating.

Lightning, or electrical breakdown is a result of large 
electric field $E$. Electric field $E$ in protoplanetary disk can be generated by
magnetorotational instability (MRI) or by the collective motion of charged dust.
The breakdown model sets an upper limit $E \leq E_{\rm crit}$ to the electric field amplitude.
At the point $E = E_{\rm crit}$ electric discharge takes place, which increases the ionization degree of the medium
and prevents the further growth of the electric field amplitude. 
Thus, electric field amplitude is kept under the upper limit ($E \leq E_{\rm crit}$) .

Lightning bolts, contrary to intuition, do not serve as the only observational signals emitters.
This is because lightning is transient events, and typical radius of a lightning bolt is $5\times 10^3$ times
mean free path
@{citep ["bibcode:1992ApJ...387..364P"]}. This radius
is much smaller than the scale height of the disk. Hence 
even if the critical condition is met, most of the time
most of the protoplanetary disk gas is in the region
outside the lightning bolts. 
We call this lightning matrix gas (LMG).
Properties of LMG is no different from those of the disk gas without lightning,
but differ in one point that LMG is subject to critical
electric field $E \simeq E_{\rm crit}$. In this paper, we explore the possible observational features of the LMG.

This paper is organized as follows. In section \ref{sec:Model}, we introduce the 
discharge models, taking the Earth atposphere as an example (\S \ref{sec:DischargeModel}, \ref{sec:DischargeAir}); 
introduce the protoplanetary disk model (\S \ref{sec:DiskModel}); apply the discharge model to the disk gas
(\S \ref{sec:DiskDischargeModel}).
In section \ref{sec:Observation} we establish our observation model
by first calculating the terminal velocity of the ion molecules
\S \ref{sec:ObservationLines}, then estimating the spectral irradiance
\S \ref{sec:ObservationEstimates}, then constructing integral maps by radiative transfer simulations 
\S \ref{sec:ObservationProfiles}. Given the simulated observational signals, we estimate the measure-of-sensitivity by
matched filtering (\S \ref{sec:MatchedFilter}).
Finally, in section \ref{sec:Observation}, we conclude and discuss the future directions of this research.
 |]

sectionConclusion :: MonadAuthoring s w m => m ()
sectionConclusion = do
  let
      keithWardle   = citep ["bibcode:2014MNRAS.440...89K"]

  command1 "section" $ esc "CONCLUSIONS AND DISCUSSIONS."
  [rawQ|   
   Discharge phenomena take place in the regions with the critical
   electric field (LMG), and we have established observable features for detecting LMGs by
   the line observations of the accelerated molecular ions.
   Dielectric strength of the disk gas, being one of the crucial elementary processes,
   will open up the understanding of the MRI in protoplanetary disks. 
   Understanding of the MRI in weakly-ionized accretion disks will
   contribute to the study of the dynamics of protoplanetary disks
   as well as circumplanetary disks @{keithWardle}.

   We have presented three dielectric strength models for protoplanetary disks.
   They are Townsend breakdown model, Druyversteyn-Penning breakdown model,
   and runaway breakdown model, respectively.
   We have proposed a method for observational distinguishment of the three models.
   The models are distinguishable with
   the sensitivity of advanced telescopes such as ALMA.
   It is now possible to reject some of the lightning models based on ground observations.
   The upper limits of the LMG clouds size are given from the observations.

   Our lightning models treated here are quite simple. Further studies are targeting to apply this work to more realistic
   disk models as well as more detailed discharge models @{citep ["arxiv:1407.8110"]}.


   |]
