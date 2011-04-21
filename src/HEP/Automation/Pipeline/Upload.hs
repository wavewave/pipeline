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


upload :: (Model a) => String -> PipelineSingleWork a 
upload ext wdav ws = do  
  let rname = makeRunName (ws_psetup ws) (ws_rsetup ws)
      filename = rname ++ ext
      ssetup = ws_ssetup ws 
      psetup = ws_psetup ws 
      wbase = workbase ssetup 
      wname = workname psetup
      ldir = wbase </> wname </> "Events" 
  uploadFile wdav (ws_storage ws) (ldir </> filename)
  
upload_PartonLHEGZ :: (Model a) => PipelineSingleWork a 
upload_PartonLHEGZ = upload "_unweighted_events.lhe.gz"

upload_PythiaLHEGZ :: (Model a) => PipelineSingleWork a 
upload_PythiaLHEGZ = upload "_pythia_events.lhe.gz"

upload_LHCO :: (Model a) => PipelineSingleWork a
upload_LHCO = upload "_pgs_events.lhco"

upload_BannerTXT :: (Model a) => PipelineSingleWork a 
upload_BannerTXT = upload "_banner.txt"


