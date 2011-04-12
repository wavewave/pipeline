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
import HEP.Automation.Pipeline.EventGeneration
import HEP.Automation.Pipeline.FileRepository

import Paths_pipeline

import qualified Data.ByteString.Lazy.Char8 as B 
import qualified Data.Binary as Bi
import qualified Data.Binary.Get as G
-- import qualified Data.ListLike as LL 
import qualified Data.Iteratee as Iter 


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
          lst <- makePhyEventClassifiedList wdir x 
          r <- eventFeedToIteratee lst testcount 
--               Iter.length `Iter.enumPair` iter_count_marker1000 
          putStrLn $ show r
          
          
--- new fetch function using iteratee 

-- safeget
{- 
iterFetchItems :: (Bi.Binary a) => 
                  Iter.Iteratee B.ByteString m [a]
iterFetchItems = Iter.liftI (step [] ) 
  where 
    step acc (Iter.Chunk xs) 
      | LL.null xs = Iter.icont (step acc) Nothing
    step acc (Iter.Chunk xs) = 
      let acc' = do 
            result <- Bi.get 
            return 
  
  -- undefined -- G.runGet onefetch       -}



pipelineEvGen  :: (Model a) => 
                  String              -- ^ system config  
                  -> String           -- ^ user config
                  -> EventGenerationSwitch 
                  -> (EventGenerationSwitch -> WorkIO a ())      -- ^ command
                  -> [ProcessSetup a] -- ^ process setup list 
                  -> ( ScriptSetup -> ClusterSetup -> [WorkSetup a] )  
                      -- ^ tasklist gen function 
                  -> IO ()
pipelineEvGen confsys confusr egs command psetuplist tasklistf = do 
  confresult <- parseConfig confsys confusr
  case confresult of 
    Left errormsg -> do 
      putStrLn errormsg
    Right (ssetup,csetup) -> do 
      -- create working directory (only once for each process)
      case dirGenSwitch egs of 
        True -> mapM_ (createWorkDir ssetup) psetuplist
        False -> return ()
      sleep 2
      mapM_ (runReaderT (command egs)) (tasklistf ssetup csetup)

       

          