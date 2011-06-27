module HEP.Automation.Pipeline.Job where

import Control.Monad.Reader

import HEP.Storage.WebDAV 
import HEP.Storage.WebDAV.Type

import HEP.Automation.Pipeline.Config
import HEP.Automation.JobQueue.JobType
import HEP.Automation.JobQueue.JobQueue

import System.Directory
import System.FilePath

import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.UserCut

data WorkConfig = WorkConfig {
  wc_localconf :: LocalConfiguration, 
  wc_webdavconf :: WebDAVServer
}

data PipelineJob = PipelineJob { 
    pipeline_checkSystem :: WorkConfig -> JobInfo -> IO Bool, 
    pipeline_startWork   :: WorkConfig -> JobInfo -> IO Bool, 
    pipeline_startTest   :: WorkConfig -> JobInfo -> IO Bool, 
    pipeline_uploadWork  :: WorkConfig -> JobInfo -> IO Bool
}


testJob :: PipelineJob
testJob = PipelineJob 
            testJob_checkSystem 
            testJob_startWork
            testJob_startTest
            testJob_uploadWork

jobMatch :: JobInfo -> PipelineJob
jobMatch jinfo = case jobinfo_detail jinfo of
                   EventGen _ _ -> testJob
                   _ -> undefined 
                    

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
testJob_startWork wc jinfo = do  
  let ss = lc_scriptSetup . wc_localconf $ wc
      cs = case (lc_smpConfiguration . wc_localconf) wc of
             SingleCPU -> CS NoParallel
             MultiCPU n -> CS (Parallel n)
      storage = (jobdetail_remotedir . jobinfo_detail) jinfo
  case (jobdetail_evset . jobinfo_detail) jinfo of 
    EventSet ps rs -> do 
      let wsetup = WS ss ps rs cs storage 
      flip runReaderT wsetup $ do 
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
    _ -> return False

testJob_startTest :: WorkConfig -> JobInfo -> IO Bool
testJob_startTest wc jinfo = return True

testJob_uploadWork :: WorkConfig -> JobInfo -> IO Bool
testJob_uploadWork wc jinfo = return True


{-
uploadEvent :: (Model a) => String -> PipelineSingleWork a 
uploadEvent ext wdav ws = upload ext (getMCDir ws) wdav ws 

upload :: (Model a) => String -> FilePath -> PipelineSingleWork a 
upload ext ldir wdav ws = do  
  let rname = makeRunName (ws_psetup ws) (ws_rsetup ws)
      filename = rname ++ ext
  uploadFile wdav (ws_storage ws) (ldir </> filename)

upload_PartonLHEGZ :: (Model a) => PipelineSingleWork a 
upload_PartonLHEGZ wdav ws = upload "_unweighted_events.lhe.gz" (getMCDir ws) wdav ws 

upload_WeightedLHEGZ :: (Model a) => PipelineSingleWork a 
upload_WeightedLHEGZ wdav ws = upload "_events.lhe.gz" (getMCDir ws) wdav ws 

upload_PythiaLHEGZ :: (Model a) => PipelineSingleWork a 
upload_PythiaLHEGZ wdav ws = upload "_pythia_events.lhe.gz" (getMCDir ws) wdav ws

upload_LHCO :: (Model a) => PipelineSingleWork a
upload_LHCO wdav ws = upload "_pgs_events.lhco" (getMCDir ws) wdav ws

upload_BannerTXT :: (Model a) => PipelineSingleWork a 
upload_BannerTXT wdav ws = upload "_banner.txt" (getMCDir ws) wdav ws

getMCDir :: (Model a) => WorkSetup a -> String
getMCDir ws = 
  let ssetup = ws_ssetup ws 
      psetup = ws_psetup ws 
  in  (workbase ssetup) </> (workname psetup) </> "Events"
     
-}