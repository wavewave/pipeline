module HEP.Automation.Pipeline.Job.MathAnal where

import HEP.Automation.Pipeline.Job
import HEP.Automation.JobQueue.JobQueue

import Control.Monad.Identity
import HEP.Util.Coroutine


mathanalJob :: PipelineJob 
mathanalJob = PipelineJob 
                mathanalJob_checkSystem
                mathanalJob_startWork
                mathanalJob_startTest
                mathanalJob_uploadWork

mathanalJob_checkSystem :: WorkConfig -> JobInfo -> IO Bool 
mathanalJob_checkSystem = undefined 

mathanalJob_startWork :: WorkConfig -> JobInfo -> IO Bool 
mathanalJob_startWork = undefined

mathanalJob_startTest :: WorkConfig -> JobInfo -> IO Bool
mathanalJob_startTest = undefined 

mathanalJob_uploadWork :: WorkConfig -> JobInfo -> IO Bool 
mathanalJob_uploadWork = undefined
