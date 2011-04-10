module HEP.Automation.Pipeline.Config where

import Text.Parsec 
import HEP.Automation.MadGraph.Cluster
import HEP.Automation.MadGraph.SetupType 

type ConfigParsec = Parsec String ()

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