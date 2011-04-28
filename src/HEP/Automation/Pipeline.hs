{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}

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

import HEP.Storage.WebDAV

import HEP.Automation.Pipeline.Type
import HEP.Automation.Pipeline.Config 
import HEP.Automation.Pipeline.DetectorAnalysis
import HEP.Automation.Pipeline.EventGeneration
import HEP.Automation.Pipeline.TopAFB
import HEP.Automation.Pipeline.PartonLevelAnalysis
import HEP.Automation.Pipeline.TopRecon

import HEP.Automation.Pipeline.Download

import Paths_pipeline

import qualified Data.ByteString.Lazy.Char8 as B 
import qualified Data.Binary as Bi
import qualified Data.Binary.Get as G
-- import qualified Data.ListLike as LL 
import qualified Data.Iteratee as Iter 

import HEP.Parser.LHEParser


pipelineGen :: (Model a) => PipelineSingleWork a -> PipelineWork a 
pipelineGen work wdav confsys confusr tasklistf   = do
  confresult <- parseConfig confsys confusr
  case confresult of 
    Left errormsg -> do 
      putStrLn errormsg
    Right (ssetup,csetup) -> do 
      putStrLn "top afb" 
      let tasklist = tasklistf ssetup csetup 
      templdir <- return . ( </> "template" ) =<< getDataDir 
      forM_ tasklist (work wdav)
             
pipelineTopRecon :: (Model a) => (WorkSetup a -> IO ()) -> PipelineWork a
pipelineTopRecon analysis = pipelineGen (reconAnalysis analysis)

pipelineLHE :: (Model a) => (LHEvent -> IO ()) -> PipelineWork a
pipelineLHE analysis = pipelineGen (testLHE analysis)  



pipelineTopAFB :: (Model a) => 
                  String   -- ^ system config
                  -> String -- ^ user config 
                  -> (ScriptSetup -> ClusterSetup a -> [WorkSetup a] ) 
                     -- ^ tasklistf 
                  -> WebDAVConfig
                  -> AnalysisWorkConfig 
                  -> IO () 
pipelineTopAFB confsys confusr tasklistf wdav aconf = do 
  confresult <- parseConfig confsys confusr
  case confresult of 
    Left errormsg -> do 
      putStrLn errormsg
    Right (ssetup,csetup) -> do 
      putStrLn "top afb" 
      let tasklist = tasklistf ssetup csetup 
          wdir = anal_workdir aconf 
      templdir <- return . ( </> "template" ) =<< getDataDir 
      forM_ tasklist $
        \x -> do 
          download_PartonLHEGZ wdir wdav x 
          download_BannerTXT wdir wdav x 
          let rname = makeRunName (ws_psetup x) (ws_rsetup x)
              lhefilename = rname ++ "_unweighted_events.lhe.gz"
              bannerfilename = rname ++ "_banner.txt"
              exportfilenameTopInfo = rname ++ "_topinfo.dat"
              exportfilenameSuccintInfo = rname ++ "_succintinfo2.dat"
              afb = TopAFBSetup {
                        afb_mainpkgfile = "mainTopInfoRoutine.m"
                      , afb_topinfoexportpkgfile = "topInfoExport.m"
                      , afb_lhefile = lhefilename
                      , afb_bannerfile = bannerfilename
                      , afb_exportfileTopInfo = exportfilenameTopInfo 
                      , afb_exportfileSuccintInfo = exportfilenameSuccintInfo
                      }
          topAFBSetup afb templdir wdir 
          topAFBRunMathematica afb wdir 

pipelineLHCOAnal :: (Model a) => 
                  String              -- ^ system config  
                  -> String           -- ^ user config
                  -> ( ScriptSetup -> ClusterSetup a -> [WorkSetup a] )  
                      -- ^ tasklist 
                  -> WebDAVConfig
                  -> WebDAVRemoteDir
                  -> AnalysisWorkConfig 
                  -> IO ()
pipelineLHCOAnal confsys confusr tasklistf wdav rdir aconf = do 
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
          download_LHCO wdir wdav x 
          xformLHCOtoBinary wdir x
          lst <- makePhyEventClassifiedList wdir x 
          r <- eventFeedToIteratee lst testcount 
--               Iter.length `Iter.enumPair` iter_count_marker1000 
          putStrLn $ show r
          
          


pipelineEvGen  :: (Model a) => 
                  String              -- ^ system config  
                  -> String           -- ^ user config
                  -> EventGenerationSwitch 
                  -> (EventGenerationSwitch -> WorkIO a ())      -- ^ commandInit
                  -> (EventGenerationSwitch -> WorkIO a ())      -- ^ command
                  -> ( ScriptSetup -> ClusterSetup a -> [WorkSetup a] )  
                      -- ^ tasklist gen function 
                  -> ( ScriptSetup -> ClusterSetup a -> [WorkSetup a] )  
                      -- ^ tasklist gen function 
                  -> IO ()
pipelineEvGen confsys confusr egs commandInit command inittaskf tasklistf = do 
  confresult <- parseConfig confsys confusr
  case confresult of 
    Left errormsg -> putStrLn errormsg
    Right (ssetup,csetup) -> do 
      -- create working directory (only once for each process)
      mapM_ (runReaderT (commandInit egs)) (inittaskf ssetup csetup)
      sleep 2
      mapM_ (runReaderT (command egs)) (tasklistf ssetup csetup)



pipelineEvGenCluster :: (Model a) => 
                        String              -- ^ system config  
                        -> String           -- ^ user config
                        -> (ScriptSetup -> ClusterSetup a -> ClusterWork a)
                        -> EventGenerationSwitch 
                        -> (EventGenerationSwitch -> ClusterWork a -> IO ()) -- ^ commandInit
                        -> (EventGenerationSwitch -> ClusterWork a -> IO ()) -- ^ commandEach  
                        -> IO ()
pipelineEvGenCluster confsys confusr cwgen egs commandInit commandEach = do 
  confresult <- parseConfig confsys confusr
  case confresult of 
    Left errormsg -> putStrLn errormsg
    Right (ssetup,csetup)  -> do 
      let cw = cwgen ssetup csetup 
      -- create working directory (only once for each process)
      commandInit egs cw 
      sleep 2
      commandEach egs cw
