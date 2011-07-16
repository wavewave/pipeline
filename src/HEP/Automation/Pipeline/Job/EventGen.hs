module HEP.Automation.Pipeline.Job.EventGen (
  eventgenJob 
) where

import Control.Monad.Reader
import Control.Monad.Error

import HEP.Automation.Pipeline.Job
import HEP.Automation.Pipeline.Util
import HEP.Automation.JobQueue.JobType 
import HEP.Automation.JobQueue.JobQueue
import HEP.Automation.MadGraph.Machine 
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.UserCut

import System.Directory
import System.FilePath

eventgenJob :: PipelineJob
eventgenJob = PipelineJob 
                eventgenJob_checkSystem 
                eventgenJob_startWork
                eventgenJob_startTest
                eventgenJob_uploadWork

eventgenJob_checkSystem :: WorkConfig -> JobInfo -> IO Bool
eventgenJob_checkSystem wc jinfo = doGenericWorkSetupWork wc jinfo work 
  where work ws = do 
          let ss = ws_ssetup ws 
              ps = ws_psetup ws
              wb = workbase ss
              wn = workname ps 
          b <- doesDirectoryExist (wb </> wn)
          if b 
            then return True
            else do r <- flip runReaderT ws . runErrorT $ createWorkDir ss ps
                    case r of 
                      Left errmsg -> do putStrLn errmsg 
                                        return False
                      Right _ -> return False
         
eventgenJob_startWork :: WorkConfig -> JobInfo -> IO Bool
eventgenJob_startWork wc jinfo = doGenericWorkSetupWork wc jinfo work 
  where work ws = do 
          r <- flip runReaderT ws . runErrorT $ do 
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
                 makeHepGz
--                 cleanAll
--                 cleanHepFiles
          case r of 
            Left errmsg -> do putStrLn errmsg
                              return False
            Right _     -> return True

eventgenJob_startTest :: WorkConfig -> JobInfo -> IO Bool
eventgenJob_startTest _wc _jinfo = return True

eventgenJob_uploadWork :: WorkConfig -> JobInfo -> IO Bool
eventgenJob_uploadWork wc jinfo = do 
  case uhep of 
    UploadHEP   -> doGenericWorkSetupWork wc jinfo (uploadEventFullWithHEP wdav)
    NoUploadHEP -> doGenericWorkSetupWork wc jinfo (uploadEventFull wdav)
  

  (doGenericWorkSetupWork wc jinfo cleaning :: IO Bool)
  
  where wdav = mkWebDAVConfig wc
        evset = ( jobdetail_evset . jobinfo_detail) jinfo 
        uhep = case evset of 
                 EventSet p r -> uploadhep r
                 _ -> undefined 

        cleaning ws = do 
          flip runReaderT ws . runErrorT $ cleanAll
          return True       
