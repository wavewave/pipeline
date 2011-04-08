module HEP.Automation.Pipeline.Command where

import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.SetupType

fullGeneration :: (Model a) => WorkIO a () 
fullGeneration = do 
  compileFortran
  cardPrepare                      
  generateEvents   
  runHEP2LHE       
  runHEPEVT2STDHEP 
  runPGS           
  runClean         
  updateBanner     
  cleanHepFiles
--  liftIO (sleep 5) 
