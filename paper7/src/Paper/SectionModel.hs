{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Paper.SectionModel where

import           Control.Monad.RWS
import           Data.Dynamic
import           Model.Disk.Hayashi (hayashiModelDoc)
  
import           Text.Authoring
import           Text.Authoring.TH 


sectionModel :: MonadAuthoring s w m => m ()
sectionModel = do
  command1 "section" $ raw "Model"
  
  subSectionDielectricStrength
  subSectionDischargeModel
  
  
  
[declareLabels| takahashiDischargeFormula , furuhataFormula, nushioFormula |]

-- data TakahashiDischargeFormula = TakahashiDischargeFormula deriving Typeable
-- labelTakahashiDischargeFormula :: Label
-- labelTakahashiDischargeFormula = fromValue TakahashiDischargeFormula
  
subSectionDielectricStrength :: MonadAuthoring s w m => m ()    
subSectionDielectricStrength = do  
  command1 "subsection" $ raw "Dielectric Strength of Air"  
  
  let price = 10 :: Int
      cheapStr = "cheap! it's cheap!" :: String
  
      takahashi2007 = citep ["isbn:9784130627184"] 
  
  [rawQ| Air is cheap. It costs only #{price} yen per ton @{takahashi2007}. #{cheapStr} |]
  
  [escQ| You know what! \10 = 10 yen = 0.1$ !!! omg! |]

  [rawQ| {\bf too cheap!!} |]
  
  [escQ|

Dielectric strength of an insulating material is the maximum amplitude 
of the electric field that the subject material does not cause the electric
breakdown. It is physical property of central importance for discharge 
physics.

Lightning on Earth is discharge phenomenon in the air, but it has been a long standing 
problem that lightning takes place under electric field amplitude well below the 
dielectric strength of air; The dielectric strength of air as function of pressure is
         |]
  citep ["isbn:9784130627184"] -- Takahashi 2007
  esc $ ": "
  
  environment "eqnarray" $ do
    [rawQ|
E &=& E_0 \left( \frac{P}{P_0} \right) ^ {1.65} ,
|]
    label takahashiDischargeFormula

  raw "where $E_0 = 30 {\\rm kV/cm}$ "

  citep [ "doi:10.1063/1.323084", "isbn:9780028645865"]

  [rawQ| , $P_0 = 1 {\rm atm}$ are dielectric strength and pressure 
of air at ground level, respectively. On the other hand, intracloud lightning
is observed with electric field amplitude of
  |]  

  raw "$140 {\\rm V/cm}$ "
  citep ["doi:10.1029/96JD01625"]

  raw " to "

  raw "$150 {\\rm V/cm}$ "
  citep ["doi:10.1029/JD091iD01p01231"] 
  esc ". "

  esc "Cloud-to-ground lightning is observed with "

  raw "$1 {\\rm kV/cm}$ "
  citep ["bibcode:1983JMSJ...61...656"]
  raw " to "

  raw "$1.8 {\\rm kV/cm}$ "
  citep ["bibcode:1999JAtS...56.1561T"]
  raw ". "


subSectionDischargeModel :: MonadAuthoring s w m => m ()  
subSectionDischargeModel = do
  command1 "subsection" $ raw "Breakdown Models"
  
  esc $ "We compare following three models of breakdown model: "
  
  environment "itemize" $ do

    raw "\\item[{\\tt [C]} ]"
    
    [escQ| 
           Conventional breakdown model, widely used in meteorological context,
           and also adopted into astrophysical context e.g. by |]
    citet ["doi:10.1006/icar.1999.6245", "bibcode:2010MNRAS.401.2641M"]
    raw ". This model explains Equations ("
    ref takahashiDischargeFormula
    raw ") well. \n\n"


    raw "\\item[{\\tt [DP]} ]"

    [escQ| 
           Druyversteyn-Penning breakdown model, based on electrical discharge 
           model proposed by |]
    citet ["doi:10.1103/RevModPhys.12.87"]
    esc " and introduced as a protoplanetary disk lightning model by "
    citet ["doi:10.1086/432796"]
    raw ".\n\n"


    raw "\\item[{\\tt [R]} ]"

    esc "Runaway breakdown model, proposed by "
    citet ["doi:10.1016/0375-9601(92)90348-P","doi:10.1070/PU2001v044n11ABEH000939"]
    [escQ|. Runaway breakdown occurs in a electric field an order of magnitude   
           Weaker than that of a conventional breakdown. Runaway breakdown
           better explains the lightning observations and used as the discharge
           model in thunderstorm simulations studies
        e.g. by |]
    citet ["bibcode:2002JGRD..107.4075M"]
    raw ". \n\n"
  


  esc "The interactons of electrons with even the simplest atoms and molecules "
  citet [ "isbn:3-540-64296-X" -- atoms
        , "isbn:3540653473" -- atomic ions
        , "isbn:354044338X" -- molecules
        ]
  raw ".\n\n"  


  hayashiModelDoc

