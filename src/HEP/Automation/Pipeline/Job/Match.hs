module HEP.Automation.Pipeline.Job.Match where

import HEP.Automation.Pipeline.Job
import HEP.Automation.Pipeline.Job.Dummy
import HEP.Automation.Pipeline.Job.EventGen
import HEP.Automation.Pipeline.Job.MathAnal

import HEP.Automation.JobQueue.JobQueue

jobMatch :: JobInfo -> PipelineJob
jobMatch jinfo = case jobinfo_detail jinfo of
                   EventGen _ _ -> eventgenJob
                   MathAnal "tev_reco" _ _ -> mathanalJob tevTopReco 
                   _ -> dummyJob
