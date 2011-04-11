module HEP.Automation.Pipeline.Config where

import Text.Parsec 
import HEP.Automation.MadGraph.Cluster
import HEP.Automation.MadGraph.SetupType 

import System.FilePath ((</>))
import Paths_pipeline

type ConfigParsec = Parsec String ()

parseConfig :: String 
               -> String 
               -> IO (Either String (ScriptSetup,ClusterSetup))
parseConfig confsys confusr = do
  let sysresult = parse configEvGenSystem "" confsys
  templdir <- return . ( </> "template" ) =<< getDataDir 
  case sysresult of 
    Right f -> do 
      let usrresult = parse (configEvGenUser templdir f) "" confusr
      case usrresult of 
        Right (ssetup,csetup) -> return (Right (ssetup,csetup))
        Left errormsg -> return (Left (show errormsg))
    Left errormsg -> do 
      return (Left (show errormsg))
        


configEvGenSystem :: ConfigParsec (String -> String -> (ScriptSetup,ClusterSetup)) 
configEvGenSystem = do 
  mg5  <- p_dir "mg5base"
  work <- p_dir "workbase"
  clu  <- p_cluster 
  return (\x y -> (SS x y mg5 work, CS clu))

configEvGenUser :: String 
                   -> (String->String-> (ScriptSetup,ClusterSetup)) 
                   -> ConfigParsec (ScriptSetup,ClusterSetup)
configEvGenUser tmpldir f = do 
  wdir <- p_dir "workingdir"
  return $ f tmpldir wdir 
  
-- configLHCAnalUser :: String -> ConfigParsec () 
  



p_dir :: String -> ConfigParsec String              
p_dir str = do              
  string str 
  spaces
  char '=' 
  spaces
  val <- many1 (noneOf " \n")
  many (char ' ')
  char '\n'
  return val
  
p_cluster :: ConfigParsec ClusterRunType
p_cluster = do 
  string "cluster"
  spaces 
  char '='
  spaces
  numstr <- many1 (digit)
  many (char ' ')
  char '\n'
  let n = read numstr
  if n == 0 
    then return NoParallel 
    else return (Parallel n)