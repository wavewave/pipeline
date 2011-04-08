{-# LANGUAGE PackageImports #-}

module HEP.Automation.Pipeline where

import "mtl" Control.Monad.Reader 

import HEP.Automation.MadGraph.Run

import System.Posix.Unistd (sleep)

import HEP.Automation.MadGraph.Dataset.SUSY
import HEP.Automation.MadGraph.Dataset.Set20110307set2_1


pipeline  :: IO ()
pipeline = do 
  putStrLn "benchmark models 20110307set2_1"
  putStrLn "models : ZpHFull"
           
  let cmdSequence = do 
             compileFortran
             cardPrepare                      
             generateEvents   
             liftIO (sleep 10)
             runHEP2LHE       
             liftIO (sleep 10)
             runHEPEVT2STDHEP 
             liftIO (sleep 10)
             runPGS           
             liftIO (sleep 10) 
             runClean         
             liftIO (sleep 10) 
             updateBanner     
             --                cleanHepFiles
             liftIO (sleep 5) 
       
  -- create working directory (only once for each process)
  mapM_ (createWorkDir my_ssetup) psetuplist
  sleep 2
  mapM_ (runReaderT cmdSequence) totaltasklist 

          