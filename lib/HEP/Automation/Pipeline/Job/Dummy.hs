module HEP.Automation.Pipeline.Job.Dummy (
  dummyJob
) where

import HEP.Automation.Pipeline.Job
import HEP.Automation.JobQueue.JobQueue

dummyJob :: PipelineJob
dummyJob = PipelineJob
             dummyJob_checkSystem
             dummyJob_startWork
             dummyJob_startTest
             dummyJob_uploadWork

dummyJob_checkSystem :: WorkConfig -> JobInfo -> IO Bool
dummyJob_checkSystem _ _ = return True

dummyJob_startWork :: WorkConfig -> JobInfo -> IO Bool
dummyJob_startWork _ _ = return True

dummyJob_startTest :: WorkConfig -> JobInfo -> IO Bool
dummyJob_startTest _ _ = return True

dummyJob_uploadWork :: WorkConfig -> JobInfo -> IO Bool
dummyJob_uploadWork _ _ = return True 
