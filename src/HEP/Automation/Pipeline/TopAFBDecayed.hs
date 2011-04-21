module HEP.Automation.Pipeline.TopAFBDecayed where

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
import HEP.Automation.MadGraph.Model 

import HEP.Automation.Pipeline.Type
import HEP.Automation.Pipeline.Download
import HEP.Automation.Pipeline.DetectorAnalysis



import Paths_pipeline

data TopAFBDecayedSetup = TopAFBDecayedSetup { 
      afbd_mainPkgFile                     :: FilePath
    , afbd_lheLepJetCuttingFunctionPkgFile :: FilePath
    , afbd_lheFile                         :: FilePath
    , afbd_bannerFile                      :: FilePath
    , afbd_exportFileOne                   :: FilePath
    , afbd_exportFileTwo                   :: FilePath
    , afbd_exportFileThree                 :: FilePath
    } deriving (Show)

topAFBDecayedSetup :: TopAFBDecayedSetup 
                   -> FilePath      -- ^ Template Path
                   -> FilePath      -- ^ Working Directory
                   -> IO () 
topAFBDecayedSetup afb templatedir workingdir = do 
  templates <- directoryGroup templatedir
  let str = (renderTemplateGroup 
              templates
              [ ("lheLepJetCuttingFunctionPkgFile" , afbd_lheLepJetCuttingFunctionPkgFile afb)
              , ("lheFile"                         , afbd_lheFile afb)
              , ("bannerFile"                      , afbd_bannerFile afb)
              , ("exportFileOne"                   , afbd_exportFileOne afb)
              , ("exportFileTwo"                   , afbd_exportFileTwo  afb)
              , ("exportFileThree"                 , afbd_exportFileThree  afb)
              ] 
              (afbd_mainPkgFile afb)) ++ "\n\n\n" 

  copyFile (templatedir </> afbd_lheLepJetCuttingFunctionPkgFile afb) 
           (workingdir </> afbd_lheLepJetCuttingFunctionPkgFile afb)
  
  existThenRemove (workingdir </> afbd_mainPkgFile afb)
  writeFile (workingdir </> afbd_mainPkgFile afb) str  
  return () 

topAFBDecayedRunMathematica :: TopAFBDecayedSetup
                            -> FilePath      -- ^ Working Directory
                            -> IO () 
topAFBDecayedRunMathematica afb wdir = do
  setCurrentDirectory wdir 
  checkFile (wdir </> afbd_mainPkgFile afb) 5
  checkFile (wdir </> afbd_lheLepJetCuttingFunctionPkgFile afb) 5
  h_pkgstr <- openFile (wdir </> afbd_mainPkgFile afb) ReadMode 
  pkgstr <- hGetContents h_pkgstr
  outstr <- readProcess "/Applications/Mathematica.app/Contents/MacOS/MathKernel" [ ] pkgstr 
  writeFile (wdir </> (afbd_exportFileOne afb ++ ".log")) outstr


data AnalysisType = DoubleTop | SingleTop 

topAFBDecayedMathematicaDriver :: (Model a) => AnalysisWorkConfig -> AnalysisType -> PipelineSingleWork a 
topAFBDecayedMathematicaDriver aconf atype wdav ws = do
  let wdir = anal_workdir aconf
  download_PartonLHEGZ wdir wdav ws
  download_BannerTXT wdir wdav ws
  templdir <- return . ( </> "template" ) =<< getDataDir  

  let rname = makeRunName (ws_psetup ws) (ws_rsetup ws)
      lhefilename = rname ++ "_unweighted_events.lhe.gz"
      bannerfilename = rname ++ "_banner.txt"
      exportfilenameTopInfo = rname ++ "_topinfo.dat"
      exportfilenameSuccintInfo = rname ++ "_succintinfo.dat"
      afb = TopAFBDecayedSetup {
               afbd_mainPkgFile = "mainTopInfoRoutine.m"
             , afbd_lheLepJetCuttingFunctionPkgFile = case atype of 
                                                        DoubleTop -> "mainDecayedTopInfoRoutine.m"
                                                        SingleTop -> "mainDecayedSingleTopInfoRoutine.m"
             , afbd_lheFile         = rname ++ "_unweighted_events.lhe.gz"
             , afbd_bannerFile      = rname ++ "_banner.txt" 
             , afbd_exportFileOne   = rname ++ "_uncuttopinfo.dat"
             , afbd_exportFileTwo   = rname ++ "_cuttopinfo.dat"
             , afbd_exportFileThree = rname ++ "_bininfo.dat" 
             }
  topAFBDecayedSetup afb templdir wdir 
  topAFBDecayedRunMathematica afb wdir 
