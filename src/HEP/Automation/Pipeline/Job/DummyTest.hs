module HEP.Automation.Pipeline.Job.DummyTest (
  dummyTestJob
) where

import HEP.Automation.Pipeline.Job
import HEP.Automation.JobQueue.JobQueue

import Control.Concurrent

dummyTestJob :: PipelineJob
dummyTestJob = PipelineJob
             dummyTestJob_checkSystem
             dummyTestJob_startWork
             dummyTestJob_startTest
             dummyTestJob_uploadWork

dummyTestJob_checkSystem :: WorkConfig -> JobInfo -> IO Bool
dummyTestJob_checkSystem _ _ = do 
  threadDelay (20*1000000)
  return True

dummyTestJob_startWork :: WorkConfig -> JobInfo -> IO Bool
dummyTestJob_startWork _ _ = do 
  threadDelay (20*1000000)
  return True

dummyTestJob_startTest :: WorkConfig -> JobInfo -> IO Bool
dummyTestJob_startTest _ _ = do 
  threadDelay (20*1000000)
  return True

dummyTestJob_uploadWork :: WorkConfig -> JobInfo -> IO Bool
dummyTestJob_uploadWork _ _ = do
  threadDelay (20*1000000)
  return True 
