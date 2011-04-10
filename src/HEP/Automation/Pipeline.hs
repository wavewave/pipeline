module HEP.Automation.Pipeline where

import Control.Monad.Reader 

import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.SetupType

import System.Posix.Unistd (sleep)
import System.FilePath ((</>))

import Text.Parsec 
import HEP.Automation.Pipeline.Config 

import Paths_pipeline

pipelineLHCOAnal :: (Model a) => 
                    String 
                    -> String 
                    -> WorkSetup a 
                    -> IO () 
pipelineLHCOAnal gconfig lconfig ws = do 
  putStrLn "LHCAnalysis" 

pipelineEvGen  :: (Model a) => 
                  String              -- ^ system config  
                  -> String           -- ^ user config
                  -> WorkIO a ()      -- ^ command
                  -> [ProcessSetup a] -- ^ process setup list 
                  -> ( ScriptSetup -> ClusterSetup -> [WorkSetup a] )  
                      -- ^ tasklist 
                  -> IO ()
pipelineEvGen confsys confusr command psetuplist tasklist = do 
  let sysresult = parse configEvGenSystem "" confsys
  templdir <- return . ( </> "template" ) =<< getDataDir 
  case sysresult of 
    Right f -> do 
      let usrresult = parse (configEvGenUser templdir f) "" confusr
      case usrresult of 
        Right (ssetup,csetup) -> do 
          putStrLn $ show (ssetup,csetup)
          -- create working directory (only once for each process)
          mapM_ (createWorkDir ssetup) psetuplist
          sleep 2
          mapM_ (runReaderT command) (tasklist ssetup csetup) 
        Left something -> do 
          putStrLn $ show something
    Left something -> do 
      putStrLn $ show something 
       

          