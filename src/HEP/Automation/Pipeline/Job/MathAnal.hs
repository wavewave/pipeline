module HEP.Automation.Pipeline.Job.MathAnal where

import Control.Applicative 
import Control.Monad.Error
import Control.Monad.Trans
import Control.Monad.Reader 
import Control.Concurrent 

import HEP.Automation.Pipeline.Job
import HEP.Automation.Pipeline.Config 
import HEP.Automation.Pipeline.Util
import HEP.Automation.JobQueue.JobType
import HEP.Automation.JobQueue.JobQueue

import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Run 
import HEP.Automation.MadGraph.Util



import Text.StringTemplate
import Text.StringTemplate.Helpers


import System.Directory 
import System.FilePath 
import System.IO 
import System.Process 

import Paths_pipeline

data MathematicaJobSpec = MathematicaJobSpec { 
  math_inputfile_var_postfix :: [(String,String)],
  math_outputfile_var_postfix :: [(String,String)],
  math_library_filename :: [String], 
  math_driver_filename :: String, 
  math_stdout_postfix :: String
} 

tevTopReco = MathematicaJobSpec { 
  math_inputfile_var_postfix   = [ ( "lhcoFile"          , "_pgs_events.lhco.gz" ) ], 
  math_outputfile_var_postfix  = [ ( "cutEventsFileName" , "_CutEvts.dat" ) 
                                 , ( "recoEventsFileName", "_RecoEvts.dat" )
                                 , ( "passedEventsFileName", "_PassedEvts.dat" ) 
                                 , ( "recoInfoFileName"  , "_RecoInfo.dat" ) ], 
  math_library_filename = [ "Chameleon1_02.m" , "tev_top_reco_july8.m" ], 
  math_driver_filename = "tev_top_reco_main_routine.m", 
  math_stdout_postfix = "_tev_top_reco.log"
}

getTemplateDir :: IO FilePath  
getTemplateDir = getDataDir >>= return . ( </> "template" ) 

mathanalJob :: MathematicaJobSpec -> PipelineJob 
mathanalJob = PipelineJob <$> mathanalJob_checkSystem 
                          <*> mathanalJob_startWork
                          <*> mathanalJob_startTest
                          <*> mathanalJob_uploadWork

mathanalJob_checkSystem :: MathematicaJobSpec -> WorkConfig -> JobInfo -> IO Bool 
mathanalJob_checkSystem mjob wc jinfo = doGenericWorkSetupWork wc jinfo work 
  where work ws = do 
          let ss = ws_ssetup ws 
              ps = ws_psetup ws 
              wb = workbase ss 
              wn = workname ps 
          putStrLn "checking mathematica system" 
          let libfilelst = math_library_filename mjob
          forM_ libfilelst $ \fn -> copyFile (templatedir ss </> fn) (workingdir ss </> fn) 
          r <- flip runReaderT ws . runErrorT $ do 
                 liftIO $ setCurrentDirectory (workingdir ss)
                 mapM_ (\x -> checkFile x 5 ) libfilelst 
          case r of 
            Left errmsg -> do putStrLn errmsg
                              return False 
            Right _     -> return True 
   
mathanalJob_startWork :: MathematicaJobSpec -> WorkConfig -> JobInfo -> IO Bool 
mathanalJob_startWork mjob wc jinfo = doGenericWorkSetupWork wc jinfo work 
  where work ws = do 
          let ss = ws_ssetup ws 
              ps = ws_psetup ws 
              wb = workbase ss 
              wn = workname ps 
          let wdav = mkWebDAVConfig wc
          let runName = makeRunName (ws_psetup ws) (ws_rsetup ws)
          templates <- directoryGroup (templatedir ss) 
          let inputs = map (\(x,y) -> (x, workingdir ss </> runName ++ y )) 
                           (math_inputfile_var_postfix mjob) 
          let outputs = map (\(x,y) -> (x, workingdir ss </> runName ++ y ))
                           (math_outputfile_var_postfix mjob)
          let str = renderTemplateGroup 
                      templates 
                      (inputs ++ outputs)
                      (math_driver_filename mjob)     
          writeFile (workingdir ss </> math_driver_filename mjob) str 

          putStrLn $ show wdav 
          mapM_ (\(_,ext) -> download wdav ws ext)
                (math_inputfile_var_postfix mjob)

          let downloadedFileNames = map (\(_,ext) -> runName ++ ext) 
                                        (math_inputfile_var_postfix mjob)
          let outputFileNames = map (\(_,ext) -> runName ++ ext) 
                                    (math_outputfile_var_postfix mjob)

          let math = mc_mathematicaPath . lc_mathematicaConfiguration . wc_localconf $ wc 
              stdout_filename  = runName ++ (math_stdout_postfix mjob)
          r <- flip runReaderT ws . runErrorT $ do 
                 liftIO $ setCurrentDirectory (workingdir ss)
                 mapM_ (flip checkFile 5) downloadedFileNames
                 liftIO $ system $ math ++ " < " 
                                        ++ math_driver_filename mjob
                                        ++ " > " 
                                        ++ stdout_filename  
 
          case r of 
            Left errmsg -> do putStrLn errmsg
                              return False 
            Right _     -> return True 
 
mathanalJob_startTest :: MathematicaJobSpec -> WorkConfig -> JobInfo -> IO Bool
mathanalJob_startTest mjob wc jinfo = doGenericWorkSetupWork wc jinfo work
  where work ws = do
          let ss = ws_ssetup ws 
              ps = ws_psetup ws 
          let runName = makeRunName (ws_psetup ws) (ws_rsetup ws)
          let outputFileNames = map (\(_,ext) -> runName ++ ext) 
                                    (math_outputfile_var_postfix mjob)
          r <- flip runReaderT ws . runErrorT $ do 
                 liftIO $ setCurrentDirectory (workingdir ss)
                 mapM_ (flip checkFile 10) outputFileNames
          case r of 
            Left errmsg -> do putStrLn errmsg
                              return False 
            Right _     -> return True 


mathanalJob_uploadWork :: MathematicaJobSpec -> WorkConfig -> JobInfo -> IO Bool 
mathanalJob_uploadWork mjob wc jinfo = doGenericWorkSetupWork wc jinfo work
  where work ws = do 
          let ss = ws_ssetup ws 
              ps = ws_psetup ws 
          let wdav = mkWebDAVConfig wc
          let runName = makeRunName (ws_psetup ws) (ws_rsetup ws)
          mapM_ (\(_,ext) -> upload wdav ws ext (workingdir ss)) 
                (math_outputfile_var_postfix mjob)
          return True 