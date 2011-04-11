{-# LANGUAGE NoMonomorphismRestriction #-}

module HEP.Automation.Pipeline.DetectorAnalysis where

import HEP.Automation.MadGraph.Util
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.SetupType

import HEP.Automation.Pipeline.WebDAV
import HEP.Automation.Pipeline.FileRepository


import HEP.Parser.LHCOAnalysis
import HEP.Parser.LHCOAnalysis.Cuts
import HEP.Parser.LHCOAnalysis.PhysObj


import System.FilePath 
import System.IO
import System.Directory 

import qualified Data.Iteratee as Iter
import Data.Iteratee.Util 

data AnalysisWorkConfig = AWConfig { 
    anal_workdir :: FilePath
  }

mkSeqLst :: [a] -> [[a]] 
mkSeqLst = mkSeqLstWorker [[]] 
  where mkSeqLstWorker :: [[b]] -> [b] -> [[b]]
        mkSeqLstWorker yss [] = yss
        mkSeqLstWorker yss (x:xs) = 
          let newelem = last yss ++ [x] 
          in  mkSeqLstWorker (yss ++ [newelem] ) xs 

apply :: (Monad m) => 
         (a -> Bool) -> Iter.Iteratee [a] m b 
         -> Iter.Iteratee [a] m b
apply cut = jn . (filtre cut)

mapply :: (Monad m) => 
          [ a -> Bool ] -> Iter.Iteratee [a] m b 
          -> Iter.Iteratee [a] m b 
mapply = apply . checkall_cuts 


mkSeqCuts lst iter = let seqlst = mkSeqLst lst 
                         iters = map (flip mapply iter) seqlst 
                     in  foldr (<:>) (return []) iters 
-- count = Iter.length 



testcuts = [ cut_missing_pt 50.0 , cut_single_lepton ] 

--testcount = mapply testcuts count <+> iter_count_marker1000

testcount = mkSeqCuts testcuts count <+> count_marker 1000 0 

--testcount = (count <+> apply testcut count <+> iter_count_marker1000)




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

makePhyEventClassifiedList :: (Model a) => 
                              FilePath 
                              -> WorkSetup a 
                              -> IO [PhyEventClassified]
makePhyEventClassifiedList wdir ws = do
  let rname = makeRunName (ws_psetup ws) (ws_rsetup ws)
      binaryname = rname ++ "_pgs_events.binary"
      filename = wdir </> binaryname
  handle <- openFile filename ReadMode
  readbyte handle 


eventFeedToIteratee :: (Monad m) => [PhyEventClassified] -> Iter.Iteratee [PhyEventClassified] m a -> m a 
eventFeedToIteratee lst iter = do
  Iter.run . Iter.joinIM $ Iter.enumPureNChunk lst 1000 iter 

iter_count_marker1000 = iter_count_marker 1000 0 