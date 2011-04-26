module HEP.Automation.Pipeline.EventGeneration where

import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.UserCut
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.SetupType

import Control.Monad 
import Control.Monad.Reader

data EventGenerationSwitch = EGS {
    dirGenSwitch  :: Bool
  , cleanUp       :: Bool
  }

clusterInit :: (Model a) => EventGenerationSwitch -> ClusterWork a ->  IO () 
clusterInit (EGS dirgen _) cw = do 
  when dirgen $ do 
    putStrLn "build template montecarlo directory"
    let (WS ssetup psetup rsetup csetup storage) = master cw  
    createWorkDir ssetup psetup 
  
    let masterworkname = workname psetup 
  
    putStrLn "copy template to each" 
    let wss = slaves cw         
    mapM_ ((replicateWorkDir masterworkname ssetup) . ws_csetup)  wss 
          

clusterEach :: (Model a) => EventGenerationSwitch -> ClusterWork a -> IO () 
clusterEach (EGS _ cleanup) cw = do 
  let action = do 
        compileFortran
        cardPrepare                      
--        generateEvents   
  putStrLn "each work"
  mapM_ (runReaderT action) (slaves cw)         
  
        
{-
  
  
  
    do WS ssetup psetup _ csetup _ <- ask 
       case cluster csetup of
         ClusterSetup  _  -> do 
           liftIO $ putStrLn "cluster setup"
--           liftIO $ createWorkDir ssetup psetup  
         _ -> error "must be cluster" 
  return () -} 
  
{-
clusterEachInit :: (Model a) => EventGenerationSwitch -> WorkIO a () 
clusterEachInit (EGS dirgen _) = do 
  when dirgen $ 
    do WS ssetup psetup _ csetup _ <- ask 
       case cluster csetup of 
         ClusterSetup _ -> do 
           liftIO $ putStrLn "cluster each setup" 
         _ -> error "must be cluster" 
-}

fullGenerationInit :: (Model a) => EventGenerationSwitch -> WorkIO a () 
fullGenerationInit (EGS dirgen _) = do 
  when dirgen $
    do WS ssetup psetup _ csetup _ <- ask 
       case cluster csetup of
         Cluster  _ _  -> do 
           liftIO $ putStrLn "cluster setup"
           liftIO $ createWorkDir ssetup psetup 
         NoParallel  -> liftIO $ putStrLn "noparallel setup"
         Parallel _  -> liftIO $ putStrLn "parallel setup" 
  return () 

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

