module HEP.Automation.Pipeline where

import Control.Monad.Reader 

import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Util


import System.Posix.Unistd (sleep)
import System.FilePath ((</>))
import System.Directory

import Text.Parsec 
import HEP.Automation.Pipeline.Config 
import HEP.Automation.Pipeline.WebDAV
import HEP.Automation.Pipeline.DetectorAnalysis
import HEP.Automation.Pipeline.FileRepository

import Paths_pipeline

pipelineLHCOAnal :: (Model a) => 
                  String              -- ^ system config  
                  -> String           -- ^ user config
                  -> ( ScriptSetup -> ClusterSetup -> [WorkSetup a] )  
                      -- ^ tasklist 
                  -> WebDAVConfig
                  -> FileRepositoryInfo
                  -> AnalysisWorkConfig 
                  -> IO ()
pipelineLHCOAnal confsys confusr tasklistf wdav finfo aconf = do 
  confresult <- parseConfig confsys confusr
  case confresult of 
    Left errormsg -> do 
      putStrLn errormsg
    Right (ssetup,csetup) -> do 
      putStrLn "LHCAnalysis" 
      let tasklist = tasklistf ssetup csetup 
          wdir = anal_workdir aconf 
      forM_ tasklist $ 
        \x -> do 
          download_LHCO wdav finfo wdir x 
          xformLHCOtoBinary wdir x
      
      


pipelineEvGen  :: (Model a) => 
                  String              -- ^ system config  
                  -> String           -- ^ user config
                  -> WorkIO a ()      -- ^ command
                  -> [ProcessSetup a] -- ^ process setup list 
                  -> ( ScriptSetup -> ClusterSetup -> [WorkSetup a] )  
                      -- ^ tasklist gen function 
                  -> IO ()
pipelineEvGen confsys confusr command psetuplist tasklistf = do 
  confresult <- parseConfig confsys confusr
  case confresult of 
    Left errormsg -> do 
      putStrLn errormsg
    Right (ssetup,csetup) -> do 
      -- create working directory (only once for each process)
      mapM_ (createWorkDir ssetup) psetuplist
      sleep 2
      mapM_ (runReaderT command) (tasklistf ssetup csetup)

       

          