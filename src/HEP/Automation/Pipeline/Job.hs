{-# LANGUAGE Rank2Types #-}

module HEP.Automation.Pipeline.Job where

import Control.Monad.Reader

import HEP.Storage.WebDAV 
import HEP.Storage.WebDAV.Type

import HEP.Automation.Pipeline.Config
import HEP.Automation.JobQueue.JobType
import HEP.Automation.JobQueue.JobQueue

import System.Directory
import System.FilePath

import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.Util
import HEP.Automation.MadGraph.UserCut

import Debug.Trace

data WorkConfig = WorkConfig {
  wc_localconf :: LocalConfiguration, 
  wc_webdavconf :: WebDAVServer
} deriving Show

data PipelineJob = PipelineJob { 
    pipeline_checkSystem :: WorkConfig -> JobInfo -> IO Bool, 
    pipeline_startWork   :: WorkConfig -> JobInfo -> IO Bool, 
    pipeline_startTest   :: WorkConfig -> JobInfo -> IO Bool, 
    pipeline_uploadWork  :: WorkConfig -> JobInfo -> IO Bool
}

jobMatch :: JobInfo -> PipelineJob
jobMatch jinfo = case jobinfo_detail jinfo of
                   EventGen _ _ -> testJob
                   _ -> undefined 


dummyJob :: PipelineJob
dummyJob = PipelineJob
             dummyJob_checkSystem
             dummyJob_startWork
             dummyJob_startTest
             dummyJob_uploadWork

dummyJob_checkSystem _ _ = return True
dummyJob_startWork _ _ = return True
dummyJob_startTest _ _ = return True
dummyJob_uploadWork _ _ = return True 


testJob :: PipelineJob
testJob = PipelineJob 
            testJob_checkSystem 
            testJob_startWork
            testJob_startTest
            testJob_uploadWork

                   

testJob_checkSystem :: WorkConfig -> JobInfo -> IO Bool
testJob_checkSystem wc jinfo = do 
  let ss = lc_scriptSetup . wc_localconf $ wc  
  case jobdetail_evset . jobinfo_detail $ jinfo of
    EventSet p r -> do 
      b <- doesDirectoryExist (workbase ss </> workname p)
      if b
        then return True 
        else do 
          createWorkDir ss p   
          return True
    _ -> return False


testJob_startWork :: WorkConfig -> JobInfo -> IO Bool
testJob_startWork wc jinfo = doGenericWorkSetupWork wc jinfo work 
  where work ws = flip runReaderT ws $ do 
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
                         cleanHepFiles
                         return True
                   
doGenericWorkSetupWork :: WorkConfig -> JobInfo 
                       -> (forall a. (Model a) => WorkSetup a -> r) -> r
doGenericWorkSetupWork wc jinfo work = 
  let ss = lc_scriptSetup . wc_localconf $ wc
      cs = case (lc_smpConfiguration . wc_localconf) wc of
             SingleCPU -> CS NoParallel
             MultiCPU n -> CS (Parallel n)
      storage = (jobdetail_remotedir . jobinfo_detail) jinfo
  in  case (jobdetail_evset . jobinfo_detail) jinfo of 
       EventSet ps rs -> let wsetup = WS ss ps rs cs storage 
                         in  work wsetup 

testJob_startTest :: WorkConfig -> JobInfo -> IO Bool
testJob_startTest wc jinfo = return True

testJob_uploadWork :: WorkConfig -> JobInfo -> IO Bool
testJob_uploadWork wc jinfo = doGenericWorkSetupWork wc jinfo (uploadEventFull wdav)
    where wdav = mkWebDAVConfig wc


uploadEventFull :: (Model a) => WebDAVConfig -> WorkSetup a -> IO Bool
uploadEventFull wdav wsetup = do 
  mapM_ (uploadEvent wdav wsetup) 
    [ "_unweighted_events.lhe.gz", "_events.lhe.gz", "_pythia_events.lhe.gz"
    , "_pgs_events.lhco.gz", "_banner.txt" ]  
  return True 

uploadEvent :: (Model a) => WebDAVConfig -> WorkSetup a -> String -> IO ()  
uploadEvent wdav wsetup ext = upload wdav wsetup ext (getMCDir wsetup) 

upload :: (Model a) => WebDAVConfig -> WorkSetup a -> String -> FilePath -> IO ()
upload wdav wsetup ext ldir = do  
  let rname = makeRunName (ws_psetup wsetup) (ws_rsetup wsetup)
      filename = rname ++ ext
  uploadFile wdav (ws_storage wsetup) (ldir </> filename)


getMCDir :: (Model a) => WorkSetup a -> String
getMCDir ws = 
  let ssetup = ws_ssetup ws 
      psetup = ws_psetup ws 
  in  (workbase ssetup) </> (workname psetup) </> "Events"

mkWebDAVConfig :: WorkConfig -> WebDAVConfig
mkWebDAVConfig wc =
  let baseurl = webdav_server_url . wc_webdavconf $ wc
      wget = nc_wgetPath . lc_networkConfiguration . wc_localconf $ wc
      cadaver = nc_cadaverPath . lc_networkConfiguration . wc_localconf $ wc
  in  WebDAVConfig wget cadaver baseurl
