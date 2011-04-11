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

import Paths_pipeline

data FileRepositoryInfo = FRInfo {filedir :: FilePath}
data AnalysisWorkConfig = AWConfig { 
    anal_workdir :: FilePath
  }


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
      forM_ tasklist $ \x->download_LHCO wdav finfo wdir x 
      
      
download_LHCO :: (Model a) =>  
                 WebDAVConfig 
                 -> FileRepositoryInfo       
                 -> FilePath 
                 -> WorkSetup a 
                 -> IO () 
download_LHCO wdav finfo wdir ws = do  
  let rname = makeRunName (ws_psetup ws) (ws_rsetup ws)
      filename = rname ++ "_pgs_events.lhco"
      dirname = filedir finfo
  setCurrentDirectory wdir 
  fetchFile wdav dirname filename  
       
  
parseConfig :: String 
               -> String 
               -> IO (Either String (ScriptSetup,ClusterSetup))
parseConfig confsys confusr = do
  let sysresult = parse configEvGenSystem "" confsys
  templdir <- return . ( </> "template" ) =<< getDataDir 
  case sysresult of 
    Right f -> do 
      let usrresult = parse (configEvGenUser templdir f) "" confusr
      case usrresult of 
        Right (ssetup,csetup) -> return (Right (ssetup,csetup))
        Left errormsg -> return (Left (show errormsg))
    Left errormsg -> do 
      return (Left (show errormsg))
        

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

       

          