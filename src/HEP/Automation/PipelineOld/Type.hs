module HEP.Automation.PipelineOld.Type where

import HEP.Storage.WebDAV
import HEP.Automation.MadGraph.SetupType

type PipelineWork a = WebDAVConfig
                    -> String   -- ^ system config
                    -> String -- ^ user config 
                    -> (ScriptSetup -> ClusterSetup a -> [WorkSetup a] ) 
                    -> IO ()

type PipelineSingleWork a = WebDAVConfig -> WorkSetup a -> IO ()
