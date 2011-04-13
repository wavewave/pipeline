{-# LANGUAGE ScopedTypeVariables #-}

module HEP.Automation.Pipeline.TopRecon where

import System.Process

import Text.StringTemplate
import Text.StringTemplate.Helpers

import HEP.Automation.MadGraph.Util

data TopReconSetup = MSetup { 
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
