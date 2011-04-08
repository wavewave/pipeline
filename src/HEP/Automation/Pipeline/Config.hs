module HEP.Automation.Pipeline.Config where

import Text.Parsec 
import HEP.Automation.MadGraph.Cluster
import HEP.Automation.MadGraph.SetupType

type ConfigParsec = Parsec String ()

configSystem :: ConfigParsec (String -> String -> (ScriptSetup,ClusterSetup)) 
configSystem = do 
  mg5base     <- p_dir "mg5base"
  workbase    <- p_dir "workbase"
  cluster     <- p_cluster 
  return (\x y -> (SS x y mg5base workbase, CS cluster))

configUser :: String 
              -> (String->String-> (ScriptSetup,ClusterSetup)) 
              -> ConfigParsec (ScriptSetup,ClusterSetup)
configUser tmpldir f = do 
  workingdir <- p_dir "workingdir"
  return $ f tmpldir workingdir 
  
p_dir :: String -> ConfigParsec String              
p_dir str = do              
  string str 
  spaces
  char '=' 
  spaces
  str <- many1 (noneOf " \n")
  many (char ' ')
  char '\n'
  return str
  
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