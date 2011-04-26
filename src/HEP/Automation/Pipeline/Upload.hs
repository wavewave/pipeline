module HEP.Automation.Pipeline.Upload where

import System.Directory

import Control.Monad

import HEP.Automation.MadGraph.Util
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.SetupType

import HEP.Storage.WebDAV

import HEP.Parser.LHCOAnalysis
import HEP.Parser.LHCOAnalysis.Cuts
import HEP.Parser.LHCOAnalysis.PhysObj

import HEP.Automation.Pipeline.Type

import System.Process
import System.FilePath


import HEP.Automation.Pipeline.Download

getMCDir :: (Model a) => WorkSetup a -> String
getMCDir ws = 
  let ssetup = ws_ssetup ws 
      psetup = ws_psetup ws 
      csetup = ws_csetup ws 
      wbase = workbase ssetup 
      wname = workname psetup
  in  case  cluster csetup of 
        Cluster _ cname -> wbase </> cname </> "Events"
        _               -> wbase </> wname </> "Events" 


upload :: (Model a) => String -> FilePath -> PipelineSingleWork a 
upload ext ldir wdav ws = do  
  let rname = makeRunName (ws_psetup ws) (ws_rsetup ws)
      filename = rname ++ ext
  uploadFile wdav (ws_storage ws) (ldir </> filename)
  
uploadEvent :: (Model a) => String -> PipelineSingleWork a 
uploadEvent ext wdav ws = upload ext (getMCDir ws) wdav ws 


upload_PartonLHEGZ :: (Model a) => PipelineSingleWork a 
upload_PartonLHEGZ wdav ws = upload "_unweighted_events.lhe.gz" (getMCDir ws) wdav ws 

upload_WeightedLHEGZ :: (Model a) => PipelineSingleWork a 
upload_WeightedLHEGZ wdav ws = upload "_events.lhe.gz" (getMCDir ws) wdav ws 

upload_PythiaLHEGZ :: (Model a) => PipelineSingleWork a 
upload_PythiaLHEGZ wdav ws = upload "_pythia_events.lhe.gz" (getMCDir ws) wdav ws

upload_LHCO :: (Model a) => PipelineSingleWork a
upload_LHCO wdav ws = upload "_pgs_events.lhco" (getMCDir ws) wdav ws

upload_BannerTXT :: (Model a) => PipelineSingleWork a 
upload_BannerTXT wdav ws = upload "_banner.txt" (getMCDir ws) wdav ws


