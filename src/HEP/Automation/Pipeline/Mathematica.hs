{-# LANGUAGE ScopedTypeVariables #-}

module HEP.Automation.Pipeline.Mathematica where

import System.Process

import Text.StringTemplate
import Text.StringTemplate.Helpers

import HEP.Automation.MadGraph.Util

data MathematicaSetup = MSetup { 
    ms_packagefile         :: FilePath
  , ms_chameleondir        :: FilePath
  , ms_chameleon           :: FilePath
  , ms_lhcofile            :: FilePath
  , ms_sampleCutEvtsdat    :: FilePath
  , ms_sampleRecoEvtsdat   :: FilePath
  , ms_samplePassedEvtsdat :: FilePath
  , ms_sampleRecoInfodat   :: FilePath
  , ms_chisqrcut           :: Double
  , ms_mfile               :: FilePath
  , ms_ofile               :: FilePath
  , ms_pbsfile             :: FilePath
} deriving (Show)

mathematicaSetup :: MathematicaSetup -> FilePath -> IO () 
mathematicaSetup ms tp = do 
  templates <- directoryGroup tp 
  let str = (renderTemplateGroup 
              templates
              [ ("chameleondir"        , (ms_chameleondir        ms ))
	      , ("chameleon"           , (ms_chameleon           ms ))
              , ("lhcofile"            , (ms_lhcofile            ms ))
              , ("sampleCutEvtsdat"    , (ms_sampleCutEvtsdat    ms ))
              , ("sampleRecoEvtsdat"   , (ms_sampleRecoEvtsdat   ms )) 
              , ("samplePassedEvtsdat" , (ms_samplePassedEvtsdat ms )) 
              , ("sampleRecoInfodat"   , (ms_sampleRecoInfodat   ms ))
              , ("chisqrcut"           , show  (ms_chisqrcut           ms )) 
              ] 
              (ms_packagefile ms)) ++ "\n\n\n" 
  
  existThenRemove (ms_mfile ms)
  writeFile (ms_mfile ms) str  
  return () 

mathematicaSendBatch :: MathematicaSetup -> FilePath -> FilePath -> IO () 
mathematicaSendBatch ms tp workdir = do 
  mathematicaSetup ms tp 
  existThenRemove (ms_pbsfile ms) 

  templates <- directoryGroup tp 
  let str = (renderTemplateGroup 
              templates
              [ ("mfile"        , (ms_mfile ms ))
              , ("workdir"      , workdir)
              , ("ofile"        , (ms_ofile ms ))
              ] 
              "tev_top_reco_IW.pbs") ++ "\n\n\n" 
  writeFile (ms_pbsfile ms) str 
--  sleep 2 
  readProcess "qsub" [ms_pbsfile ms] "" 
  return () 
