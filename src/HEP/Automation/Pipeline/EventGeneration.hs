module HEP.Automation.Pipeline.EventGeneration where

import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.UserCut
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.SetupType

import Control.Monad.Reader

data EventGenerationSwitch = EGS {
    dirGenSwitch  :: Bool
  , cleanUp       :: Bool
  }

fullGenerationInit :: (Model a) => EventGenerationSwitch -> WorkIO a () 
fullGenerationInit (EGS dirgen _) = do 
  WS ssetup psetup _ csetup _ <- ask 
  case dirgen of 
    True  -> liftIO $ createWorkDir ssetup psetup
    False -> return ()
  case cluster csetup of
    NoParallel  -> liftIO $ putStrLn "noparallel setup"
    Parallel _  -> liftIO $ putStrLn "parallel setup" 
    Cluster  _  -> liftIO $ putStrLn "cluster setup"

fullGeneration :: (Model a) => EventGenerationSwitch -> WorkIO a () 
fullGeneration (EGS _ cleanup) = do 
  WS _ _ rsetup _ _ <- ask
  
  compileFortran
  cardPrepare                      
  generateEvents   
  case usercut rsetup of
    UserCutDef _ -> do 
      runHEP2LHE       
      runHEPEVT2STDHEP 
      runPGS           
      runClean         
      updateBanner     
    NoUserCutDef -> return ()
  case cleanup of
    True -> cleanHepFiles
    False -> return ()

