module HEP.Automation.Pipeline.DetectorAnalysis where

import HEP.Automation.MadGraph.Util
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.SetupType

import HEP.Automation.Pipeline.WebDAV

import HEP.Parser.LHCOAnalysis

import System.Directory 


fetchLHCOfromWorkSetup :: (Model a) => 
                          WebDAVConfig 
                          -> FilePath    -- ^ local directory 
                          -> FilePath    -- ^ remote directory
                          -> WorkSetup a -- ^ worksetup
                          -> IO () 
fetchLHCOfromWorkSetup wdav localdir remotedir ws = do 
  let runname = makeRunName (ws_psetup ws) (ws_rsetup ws) 
      lhcofilename = runname ++ "_pgs_events.lhco"

  setCurrentDirectory localdir 
  fetchFile wdav remotedir lhcofilename 
