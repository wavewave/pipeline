module HEP.Automation.Pipeline.EventGeneration where

import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.SetupType


data EventGenerationSwitch = EGS {
    dirGenSwitch :: Bool
  , usrDefinedCut :: Bool 
  }

fullGeneration :: (Model a) => EventGenerationSwitch -> WorkIO a () 
fullGeneration (EGS _ usrdef) = do 
  compileFortran
  cardPrepare                      
  generateEvents   
  case usrdef of
    True -> do  
      runHEP2LHE       
      runHEPEVT2STDHEP 
      runPGS           
      runClean         
      updateBanner     
    False -> return ()
  cleanHepFiles

