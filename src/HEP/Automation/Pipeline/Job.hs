{-# LANGUAGE Rank2Types #-}

module HEP.Automation.Pipeline.Job where

import HEP.Storage.WebDAV 
import HEP.Automation.Pipeline.Config
import HEP.Automation.JobQueue.JobQueue

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


