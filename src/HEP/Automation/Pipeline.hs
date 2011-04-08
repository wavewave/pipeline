{-# LANGUAGE PackageImports #-}

module HEP.Automation.Pipeline where

import "mtl" Control.Monad.Reader 

import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.SetupType

import System.Posix.Unistd (sleep)

import Text.Parsec 
import HEP.Automation.Pipeline.Config 
import HEP.Automation.Pipeline.Command 

pipeline  :: (Model a) => 
             String                                           -- ^ config string 
             -> WorkIO a ()                                   -- ^ command
             -> [ProcessSetup a]                              -- ^ process setup list 
             -> ( ScriptSetup -> ClusterSetup -> [WorkSetup a] )  -- ^ tasklist 
             -> IO ()
pipeline configstr command psetuplist tasklist = do 
  let parseresult = parse config "" configstr
  case parseresult of 
    Right (ssetup,csetup) -> do 
        -- create working directory (only once for each process)
      mapM_ (createWorkDir ssetup) psetuplist
      sleep 2
      mapM_ (runReaderT command) (tasklist ssetup csetup)
    Left something -> do 
      putStrLn $ show something 
       

          