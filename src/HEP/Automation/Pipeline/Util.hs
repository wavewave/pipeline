{-# LANGUAGE Rank2Types #-}

module HEP.Automation.Pipeline.Util where 

import HEP.Storage.WebDAV

import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Util

import HEP.Automation.Pipeline.Config
import HEP.Automation.Pipeline.Job
import HEP.Automation.JobQueue.JobType
import HEP.Automation.JobQueue.JobQueue

import System.FilePath


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

uploadEventFullWithHEP :: (Model a) => WebDAVConfig -> WorkSetup a -> IO Bool
uploadEventFullWithHEP wdav wsetup = do 
  mapM_ (uploadEvent wdav wsetup) 
    [ "_unweighted_events.lhe.gz", "_events.lhe.gz", "_pythia_events.lhe.gz"
    , "_pgs_events.lhco.gz", "_banner.txt", "_newbanner.txt", "_pythia.log" 
    , "_pythia_events.hep.gz" ]  
  return True 




uploadEventFull :: (Model a) => WebDAVConfig -> WorkSetup a -> IO Bool
uploadEventFull wdav wsetup = do 
  mapM_ (uploadEvent wdav wsetup) 
    [ "_unweighted_events.lhe.gz", "_events.lhe.gz", "_pythia_events.lhe.gz"
    , "_pgs_events.lhco.gz", "_banner.txt", "_newbanner.txt", "_pythia.log" ]  
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
