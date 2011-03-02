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

import HEP.Automation.MadGraph.Dataset.Set20110302_1


main :: IO ()
main = do putStrLn "benchmark models 20110302_1 sets" 
          putStrLn "models : Wprime "

	  let cmdSequence = do 
                compileFortran
                cardPrepare                      
                generateEvents   
                runHEP2LHE       
                runHEPEVT2STDHEP 
	        runPGS            
                runClean          
                updateBanner     
                cleanHepFiles
          
       
          -- create working directory (only once for each process)
          mapM_ (createWorkDir my_ssetup) psetuplist
          sleep 2
          mapM_ (runReaderT cmdSequence) totaltasklist 

          