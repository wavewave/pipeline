module HEP.Automation.Pipeline.EventGeneration where

import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.UserCut
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.Util
import HEP.Automation.MadGraph.SetupType

import Text.StringTemplate
import Text.StringTemplate.Helpers

import Control.Monad 
import Control.Monad.Reader

import System.Directory
import System.FilePath
import System.Process

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
  


clusterEach :: (Model a) => EventGenerationSwitch -> ClusterWork a -> IO () 
clusterEach (EGS dirgen cleanup) cw = do 
  let action = do 
        replication (master cw) 
        compileFortran
        cardPrepare  
        clusterGenEvent                     
--        generateEvents   

  let (WS ssetup psetup rsetup csetup storage) = master cw  

  
--   putStrLn "copy template to each" 
  let wss = slaves cw         

  putStrLn "each work"
  mapM_ (runReaderT action) wss



replication :: (Model a) => WorkSetup a -> WorkIO a () 
replication masterws = do 
  WS ssetup psetup rsetup csetup _ <- ask 
  let masterworkname = workname psetup        
  liftIO (replicateWorkDir masterworkname ssetup csetup) 


clusterGenEvent :: (Model a) => WorkIO  a () 
clusterGenEvent = do 
  (WS ssetup psetup rsetup csetup _ ) <- ask 
  wdir <- getWorkDir
  let slaveworkname = cluster_workname .cluster $ csetup 
      taskname = makeRunName psetup rsetup 
  liftIO $ do 
    setCurrentDirectory wdir 
    templates <- directoryGroup (templatedir ssetup) 
    let str = renderTemplateGroup 
                templates 
                [ ("workdir",wdir) 
                , ("runname",taskname) ] 
                "generate_event.pbs" 
              ++ "\n\n\n"
        pbsfilename = workingdir ssetup </> taskname ++ ".pbs" 
    writeFile pbsfilename str 
    putStrLn $ "send job " ++ slaveworkname
    readProcess "qsub" [ pbsfilename ] "" 
    return () 
        
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

