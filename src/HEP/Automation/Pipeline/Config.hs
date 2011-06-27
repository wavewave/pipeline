module HEP.Automation.Pipeline.Config where

import Text.Parsec
import HEP.Parser.Config
import Control.Monad.Identity
import Control.Applicative

import HEP.Automation.MadGraph.SetupType
import HEP.Automation.JobQueue.Config

import System.FilePath 
import Paths_pipeline

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
  lc_networkConfiguration :: NetworkConfiguration
} deriving (Show)


clientConfigurationParse :: ParsecT String () Identity ClientConfiguration
clientConfigurationParse =
  oneGroupFieldInput "clientconf" $  
    ClientConfiguration <$> (oneFieldInput "computerName")
                        <*> (oneFieldInput "haveMathematica" >>= return . yesNo)
                        <*> (oneFieldInput "havePBS" >>= return . yesNo)
                        <*> (oneFieldInput "canMonteCarlo" >>= return . yesNo)

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

localConfigurationParse :: FilePath -> ParsecT String () Identity LocalConfiguration
localConfigurationParse tmpldir = do 
  cc  <- clientConfigurationParse
  ss  <- scriptSetupParse tmpldir
  smp <- smpConfigurationParse
  nc  <- networkConfigurationParse
  return (LocalConfiguration cc ss smp nc)

readConfigFile :: FilePath -> IO LocalConfiguration
readConfigFile conf = do 
  putStrLn conf
  str <- readFile conf
  tmpldir <- return . ( </> "template" ) =<< getDataDir
  let r = parse (localConfigurationParse tmpldir) "" str
  case r of 
    Right result -> do putStrLn (show result) 
                       return result
    Left err -> error (show err) 

