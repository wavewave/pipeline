module HEP.Automation.Pipeline.Config where

import Text.Parsec

import Control.Monad.Identity
import Control.Applicative
import Control.Exception (bracket)

import HEP.Parser.Config
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.JobQueue.Config

import System.FilePath 
import System.IO

import Paths_pipeline

data MathematicaConfiguration = MathConf { 
  mc_mathematicaPath :: String 
} deriving (Show)

data NetworkConfiguration = NC { 
  nc_polling :: Int, 
  nc_jobqueueurl :: String, 
  nc_wgetPath :: FilePath, 
  nc_cadaverPath :: FilePath
} deriving (Show)

data LocalConfiguration = LocalConfiguration { 
  lc_clientConfiguration :: ClientConfiguration, 
  lc_scriptSetup :: ScriptSetup, 
  lc_smpConfiguration :: SMPConfiguration, 
  lc_networkConfiguration :: NetworkConfiguration, 
  lc_mathematicaConfiguration :: MathematicaConfiguration
} deriving (Show)

data TestConfiguration = TestConfiguration { 
  tc_storageurl :: String
} deriving (Show)

testConfigurationParse :: ParsecT String () Identity TestConfiguration 
testConfigurationParse = 
  oneGroupFieldInput "testconf" $
    TestConfiguration <$> (oneFieldInput "storageURL")


clientConfigurationParse :: ParsecT String () Identity ClientConfiguration
clientConfigurationParse =
  oneGroupFieldInput "clientconf" $  
    ClientConfiguration <$> (oneFieldInput "computerName")
                        <*> (oneFieldInput "haveMathematica" >>= return . yesNo)
                        <*> (oneFieldInput "havePBS" >>= return . yesNo)
                        <*> (oneFieldInput "canMonteCarlo" >>= return . yesNo)
                        <*> (oneFieldInput "datasetDir")

scriptSetupParse :: FilePath -> ParsecT String () Identity ScriptSetup
scriptSetupParse tmpldir = 
  oneGroupFieldInput "scriptconf" $ 
    SS tmpldir <$> (oneFieldInput "workingdir")
               <*> (oneFieldInput "mg5base")
               <*> (oneFieldInput "workbase")

smpConfigurationParse :: ParsecT String () Identity SMPConfiguration
smpConfigurationParse = 
  oneGroupFieldInput "smp" $ do 
     ncpu <- oneFieldInput "cpu"
     case (read ncpu) of 
       1 -> return SingleCPU
       n -> return (MultiCPU n)

networkConfigurationParse :: ParsecT String () Identity NetworkConfiguration
networkConfigurationParse =
  oneGroupFieldInput "network" $ do 
    polling <- oneFieldInput "polling"
    url <- oneFieldInput "jobqueueurl"
    wget <- oneFieldInput "wgetPath"
    cadaver <- oneFieldInput "cadaverPath"
    return (NC (read polling) url wget cadaver)

mathematicaConfigurationParse :: ParsecT String () Identity MathematicaConfiguration
mathematicaConfigurationParse =
  oneGroupFieldInput "mathematica" $ MathConf <$> oneFieldInput "mathematicaPath"


localConfigurationParse :: FilePath -> ParsecT String () Identity LocalConfiguration
localConfigurationParse tmpldir = 
  LocalConfiguration <$> clientConfigurationParse
                     <*> scriptSetupParse tmpldir
                     <*> smpConfigurationParse
                     <*> networkConfigurationParse
                     <*> mathematicaConfigurationParse 
  


readConfigFile :: FilePath -> IO LocalConfiguration
readConfigFile conf = do 
  putStrLn conf
  bracket (openFile conf ReadMode) hClose $ \fh -> do 
    str <- hGetContents fh --  readFile conf
    tmpldir <- return . ( </> "template" ) =<< getDataDir
    let r = parse (localConfigurationParse tmpldir) "" str
    case r of 
      Right result -> do putStrLn (show result) 
                         return $! result
      Left err -> error (show err) 

readTestConfigFile :: FilePath -> IO TestConfiguration
readTestConfigFile tconf = do 
  putStrLn tconf
  bracket (openFile tconf ReadMode) hClose $ \fh -> do 
    str <- hGetContents fh --  readFile tconf
    let r = parse testConfigurationParse "" str
    case r of 
      Right result -> do putStrLn (show result) 
                         return $! result
      Left err -> error (show err) 

