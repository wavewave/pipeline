module HEP.Automation.Pipeline.TopAFB where

import System.IO
import System.Directory
import System.Process
import System.FilePath ((</>))

import Text.StringTemplate
import Text.StringTemplate.Helpers


import HEP.Storage.WebDAV
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.Util

import Paths_pipeline

data TopAFBSetup = TopAFBSetup { 
      afb_mainpkgfile          :: FilePath
    , afb_topinfoexportpkgfile :: FilePath
    , afb_lhefile              :: FilePath
    , afb_bannerfile           :: FilePath
    , afb_exportfileTopInfo    :: FilePath
    , afb_exportfileSuccintInfo :: FilePath
    } deriving (Show)

topAFBSetup :: TopAFBSetup 
            -> FilePath      -- ^ Template Path
            -> FilePath      -- ^ Working Directory
            -> IO () 
topAFBSetup afb templatedir workingdir = do 
  templates <- directoryGroup templatedir
  let str = (renderTemplateGroup 
              templates
              [ ("TopInfoExportPkgFile" , afb_topinfoexportpkgfile afb)
              , ("lheFile"              , afb_lhefile afb)
              , ("bannerFile"           , afb_bannerfile afb)
              , ("exportFileTopInfo"    , afb_exportfileTopInfo afb)
              , ("exportFileSuccintInfo", afb_exportfileSuccintInfo afb)
              ] 
              (afb_mainpkgfile afb)) ++ "\n\n\n" 

  copyFile (templatedir </> afb_topinfoexportpkgfile afb) 
           (workingdir </> afb_topinfoexportpkgfile afb)
  
  existThenRemove (workingdir </> afb_mainpkgfile afb)
  writeFile (workingdir </> afb_mainpkgfile afb) str  
  return () 

topAFBRunMathematica :: TopAFBSetup
                     -> FilePath      -- ^ Working Directory
                     -> IO () 
topAFBRunMathematica afb wdir = do
  setCurrentDirectory wdir 
  checkFile (wdir </> afb_mainpkgfile afb) 5
  checkFile (wdir </> afb_topinfoexportpkgfile afb) 5
--  runCommand $
--    "/Applications/Mathematica.app/Contents/MacOS/MathKernel < " ++ afb_mainpkgfile afb ++ " > " ++ afb_exportfile afb ++ ".log"
--  return ()
  h_pkgstr <- openFile (wdir </> afb_mainpkgfile afb) ReadMode 
  pkgstr <- hGetContents h_pkgstr
--   hFlush h_pkgstr
  
  outstr <- readProcess "/Applications/Mathematica.app/Contents/MacOS/MathKernel" [ ] pkgstr 
  
  writeFile (wdir </> (afb_exportfileTopInfo afb ++ ".log")) outstr


{-
topreconSendBatch :: TopReconSetup -> FilePath -> FilePath -> IO () 
topreconSendBatch ts tp workdir = do 
  topreconSetup ts tp 
  existThenRemove (tr_pbsfile ts) 

  templates <- directoryGroup tp 
  let str = (renderTemplateGroup 
              templates
              [ ("mfile"        , (tr_mfile ts ))
              , ("workdir"      , workdir)
              , ("ofile"        , (tr_ofile ts ))
              ] 
              "tev_top_reco_IW.pbs") ++ "\n\n\n" 
  writeFile (tr_pbsfile ts) str 
--  sleep 2 
  readProcess "qsub" [tr_pbsfile ts] "" 
  return () 

-}