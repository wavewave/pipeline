module HEP.Automation.Pipeline.Download where

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

getFilename :: (Model a) => String -> WorkSetup a -> String 
getFilename ext ws = 
  let rname = makeRunName (ws_psetup ws) (ws_rsetup ws)
  in  rname ++ ext 


download :: (Model a) => String -> FilePath -> PipelineSingleWork a 
download ext wdir wdav ws = do  
  let rname = makeRunName (ws_psetup ws) (ws_rsetup ws)
      filename = rname ++ ext
  setCurrentDirectory wdir 
  fetchFile wdav (ws_storage ws) filename  

gunzip :: (Model a) => String -> FilePath -> PipelineSingleWork a 
gunzip ext wdir wdav ws = do 
  let rname = makeRunName (ws_psetup ws) (ws_rsetup ws)
      filename = rname ++ ext 
  setCurrentDirectory wdir 
  readProcess "/usr/bin/gunzip" [filename] "" 
  return ()



download_PartonLHEGZ :: (Model a) => FilePath -> PipelineSingleWork a 
download_PartonLHEGZ = download "_unweighted_events.lhe.gz"

{-
download_PartonLHEGZandUnzip :: (Model a) => PipelineSingleWork 

download_PartonLHEGZandUnzip wdav wdir ws = do
  download "_unweighted_events.lhe.gz" wdav wdir ws 
  gunzip "_unweighted_events.lhe.gz" wdav wdir ws  -}


download_LHCOGZ :: (Model a) => FilePath -> PipelineSingleWork a 
download_LHCOGZ = download "_pgs_events.lhco.gz"



download_LHCO :: (Model a) => FilePath -> PipelineSingleWork a
download_LHCO = download "_pgs_events.lhco"

download_BannerTXT :: (Model a) => FilePath -> PipelineSingleWork a 
download_BannerTXT = download "_banner.txt"


