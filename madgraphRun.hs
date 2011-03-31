{-# LANGUAGE PackageImports #-}

module Main where

import "mtl" Control.Monad.Reader 

import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.UserCut
import HEP.Automation.MadGraph.Cluster
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Run

import System.Posix.Unistd (sleep)

import HEP.Automation.MadGraph.Dataset.Desktop
import HEP.Automation.MadGraph.Dataset.Set20110330set2


main :: IO ()
main = do putStrLn "benchmark models 20110330sets"
          putStrLn "models : TripFull"

	  let cmdSequence = do 
                compileFortran
                cardPrepare                      
                generateEvents   
--                runHEP2LHE       
--                runHEPEVT2STDHEP 
--               	runPGS            
--                runClean          
--                updateBanner     
                cleanHepFiles
                liftIO (sleep 5) 
       
          -- create working directory (only once for each process)
          mapM_ (createWorkDir my_ssetup) psetuplist
          sleep 2
          mapM_ (runReaderT cmdSequence) totaltasklist 

          