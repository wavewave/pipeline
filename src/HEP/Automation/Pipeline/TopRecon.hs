{-# LANGUAGE ScopedTypeVariables #-}

module HEP.Automation.Pipeline.TopRecon where

import System.Process
import System.FilePath
import System.Directory 

import Text.StringTemplate
import Text.StringTemplate.Helpers

import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Util

import HEP.Automation.Pipeline.Type
import HEP.Automation.Pipeline.Download


data TopReconSetup = TRSetup { 
    tr_packagefile         :: FilePath
  , tr_chameleondir        :: FilePath
  , tr_chameleon           :: FilePath
  , tr_lhcofile            :: FilePath
  , tr_sampleCutEvtsdat    :: FilePath
  , tr_sampleRecoEvtsdat   :: FilePath
  , tr_samplePassedEvtsdat :: FilePath
  , tr_sampleRecoInfodat   :: FilePath
  , tr_chisqrcut           :: Double
  , tr_mfile               :: FilePath
  , tr_ofile               :: FilePath
  , tr_pbsfile             :: FilePath
} deriving (Show)


testrecon :: WorkSetup a -> IO () 
testrecon = undefined 

reconAnalysis :: (Model a) => (WorkSetup a -> IO ()) -> PipelineSingleWork a 
reconAnalysis analysis wdav ws = do 
  putStrLn "top quark reconstruction"
  let WS ssetup psetup rsetup csetup storage = ws 
      wdir = workingdir ssetup 
      tdir = templatedir ssetup
  download_LHCOGZ wdir wdav ws 
  
  let tsetup = topreconPrelim wdir tdir 100000 ws 
  topreconSetup tsetup tdir
  copyFile (tdir </> "Chameleon1_02.m") (wdir </> "Chameleon1_02.m")
  topreconSendBatch tsetup tdir wdir 



topreconPrelim :: (Model a) => FilePath -> FilePath -> Double -> WorkSetup a -> TopReconSetup
topreconPrelim workdir templatedir chisqrcut ws =  
  let psetup = ws_psetup ws
      rsetup = ws_rsetup ws 
      runname = makeRunName psetup rsetup	
      package = "tev_top_reco_IW_package.m"
      chameleondir = workdir 
      chameleon = "Chameleon1_02.m"
      lhcofile = workdir </> runname ++ "_pgs_events.lhco.gz"
      sampleCutEvtsdat    = workdir </> runname++"_sampleCutEvts_"++"ChiSqrCut"++ (show chisqrcut) ++ ".dat"
      sampleRecoEvtsdat   = workdir </> runname++"_sampleRecoEvts_"++"ChiSqrCut"++ (show chisqrcut) ++ ".dat"
      samplePassedEvtsdat = workdir </> runname++"_samplePassedEvts_"++"ChiSqrCut"++(show chisqrcut) ++ ".dat"
      sampleRecoInfodat   = workdir </> runname++"_sampleRecoInfo_"++"ChiSqrCut"++(show chisqrcut) ++".dat"
      mfile   = workdir </> runname ++ ".m"
      ofile   = workdir </> runname ++ ".log"
      pbsfile = workdir </> runname ++ ".pbs"
  in TRSetup { 
           tr_packagefile  = package,
           tr_chameleondir = chameleondir, 
           tr_chameleon    = chameleon, 
           tr_lhcofile    = lhcofile,
           tr_sampleCutEvtsdat = sampleCutEvtsdat, 
           tr_sampleRecoEvtsdat = sampleRecoEvtsdat, 
           tr_samplePassedEvtsdat = samplePassedEvtsdat, 
           tr_sampleRecoInfodat = sampleRecoInfodat, 
           tr_chisqrcut = chisqrcut,
           tr_mfile = mfile, 
           tr_ofile = ofile,
           tr_pbsfile = pbsfile
         }
  


topreconSetup :: TopReconSetup -> FilePath -> IO () 
topreconSetup ts tp = do 
  templates <- directoryGroup tp 
  let str = (renderTemplateGroup 
              templates
              [ ("chameleondir"        , (tr_chameleondir        ts ))
	      , ("chameleon"           , (tr_chameleon           ts ))
              , ("lhcofile"            , (tr_lhcofile            ts ))
              , ("sampleCutEvtsdat"    , (tr_sampleCutEvtsdat    ts ))
              , ("sampleRecoEvtsdat"   , (tr_sampleRecoEvtsdat   ts )) 
              , ("samplePassedEvtsdat" , (tr_samplePassedEvtsdat ts )) 
              , ("sampleRecoInfodat"   , (tr_sampleRecoInfodat   ts ))
              , ("chisqrcut"           , show  (tr_chisqrcut           ts )) 
              ] 
              (tr_packagefile ts)) ++ "\n\n\n" 
  
  existThenRemove (tr_mfile ts)
  writeFile (tr_mfile ts) str  
  return () 

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


