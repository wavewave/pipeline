{-# LANGUAGE OverloadedStrings #-}

module HEP.Automation.Pipeline.PartonLevelAnalysis where

import HEP.Automation.MadGraph.Util
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.SetupType

import HEP.Storage.WebDAV

import HEP.Parser.LHCOAnalysis
import HEP.Parser.LHCOAnalysis.Cuts
import HEP.Parser.LHCOAnalysis.PhysObj

import Control.Monad 

import System.FilePath 
import System.IO
import System.Directory 

import HEP.Automation.Pipeline.Type 
import HEP.Automation.Pipeline.Download 


import HEP.Parser.LHEParser
import HEP.Parser.LHEParser.Parser
import qualified Data.Attoparsec as P 
import Data.Attoparsec.Char8 hiding (takeWhile,take) 


import qualified Data.ByteString.Char8 as B

import Data.Maybe 

import Debug.Trace

onlyresult (Done _ r) = r
onlyremain (Done s _) = s 





testLHE :: (Model a) => (LHEvent -> IO ()) -> PipelineSingleWork a 
testLHE analysis wdav ws = do 
  setCurrentDirectory (workingdir (ws_ssetup ws))

  b <- doesFileExist (getFilename "_unweighted_events.lhe" ws)
  if b 
    then return () 
    else do 
      download_PartonLHEGZ (workingdir (ws_ssetup ws)) wdav ws 
      gunzip "_unweighted_events.lhe.gz" (workingdir (ws_ssetup ws)) wdav ws

  bytestr <- B.readFile (getFilename "_unweighted_events.lhe" ws)

  let r = parse untilfirstevent bytestr
  let onlyeventbstr = case r of 
        Partial _  -> trace "what" $ "<event>" `B.append` onlyremain (feed r B.empty)
        Done _  _  -> trace "test" $ "<event>" `B.append` onlyremain r
        Fail x _ _ -> undefined -- trace (show x) (onlyremain r)  

  let totalevents = (map fromJust . parseevents) onlyeventbstr
{-      hadronic = filter (\x->countLeptonStatus 1 x==0) totalevents
      semilep  = filter (\x->countLeptonStatus 1 x==1) totalevents 
      fulllep  = filter (\x->countLeptonStatus 1 x==2) totalevents  -}


  putStrLn $ " number of total events : " ++ show (length totalevents)

  mapM_ analysis totalevents

--  putStrLn $ show (head result)
{-  putStrLn $ " number of semilep events: " ++ show (length semilep)  
  putStrLn $ " number of fulllep events: " ++ show (length fulllep)
  putStrLn $ " number of hadronic events: " ++ show (length hadronic) -}



