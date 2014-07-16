{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- worker node in pipeline
--
-----------------------------------------------------------------------------

module Main where

import Control.Applicative ((<$>))
import Data.Time.Clock
import Data.Foldable (mapM_)  
import System.Environment (getArgs) 
import System.FilePath
import System.Process (readProcess, system)
-- 
import HEP.Automation.EventGeneration.Config (getConfig, EventgenConfig(..),getCredential )
import HEP.Storage.WebDAV.CURL (downloadFile)
import HEP.Storage.WebDAV.Type
-- 
import Prelude hiding (mapM_)

main :: IO ()
main = do 
  putStrLn "starting pipeline-worker"
  fp <- (!! 0) <$> getArgs 
  scr <- (!! 1) <$> getArgs 
  jn <- (!! 2) <$> getArgs 
  pipelineWork fp (ScriptName scr) (JobName jn)


newtype ScriptName = ScriptName { unScriptName :: String } deriving (Show,Eq,Ord)

newtype JobName = JobName { unJobName :: String } deriving (Show,Eq,Ord)

pipelineWork :: FilePath -> ScriptName -> JobName -> IO ()
pipelineWork fp scrname jobname = do 
  getConfig fp >>= 
    mapM_ (\ec -> do 
            let ssetup = evgen_scriptsetup ec 
                whost = evgen_webdavroot ec
                pkey = evgen_privatekeyfile ec
                pswd = evgen_passwordstore ec 
            Just cr <- getCredential pkey pswd 
            let wdavcfg = WebDAVConfig { webdav_credential = cr 
                                       , webdav_baseurl = whost } 
            prepareScript wdavcfg scrname
            runJob wdavcfg fp scrname jobname
            return () )

prepareScript :: WebDAVConfig -> ScriptName -> IO () 
prepareScript wdavcfg scrname = do 
    downloadFile False wdavcfg (WebDAVRemoteDir "script") (unScriptName scrname <.> "hs")
    compile scrname 

runJob :: WebDAVConfig -> FilePath -> ScriptName -> JobName -> IO () 
runJob wdavcfg configfp scrname jobname = do 
    downloadFile False wdavcfg (WebDAVRemoteDir "job") (unJobName jobname <.> "json")
    runcode configfp scrname jobname  

compile :: ScriptName -> IO ()
compile scrname = do 
    let srcfile = (unScriptName scrname) <.> "hs" 
        exefile = (unScriptName scrname) <.> "exe"
    putStrLn ("I am compiling " ++ srcfile ++ " and make a binary file " ++ exefile)  
    readProcess "ghc" [ "-o", exefile, srcfile ] "" 
    return () 

runcode :: FilePath -> ScriptName -> JobName -> IO () 
runcode configfp scrname jobname = do 
    let exefile = (unScriptName scrname) <.> "exe" 
        jobfile = (unJobName jobname) <.> "json" 
    putStrLn $ " run code : " ++ exefile ++ " for job : " ++ jobfile 
    system ("." </> exefile ++ " " ++ configfp ++ " " ++ jobfile)  -- assumes there is no space . this is bad
    return () 

            
