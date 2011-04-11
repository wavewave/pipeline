module HEP.Automation.Pipeline.DetectorAnalysis where

import HEP.Automation.MadGraph.Util
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.SetupType

import HEP.Automation.Pipeline.WebDAV
import HEP.Automation.Pipeline.FileRepository


import HEP.Parser.LHCOAnalysis

import System.Directory 

data AnalysisWorkConfig = AWConfig { 
    anal_workdir :: FilePath
  }


download_LHCO :: (Model a) =>  
                 WebDAVConfig 
                 -> FileRepositoryInfo       
                 -> FilePath 
                 -> WorkSetup a 
                 -> IO () 
download_LHCO wdav finfo wdir ws = do  
  let rname = makeRunName (ws_psetup ws) (ws_rsetup ws)
      filename = rname ++ "_pgs_events.lhco"
      dirname = filedir finfo
  setCurrentDirectory wdir 
  fetchFile wdav dirname filename  

  
xformLHCOtoBinary wdir ws = do 
  let rname = makeRunName (ws_psetup ws) (ws_rsetup ws)
      infile = rname ++ "_pgs_events.lhco"
      outfile = rname ++ "_pgs_events.binary"
  makebinary infile outfile 
