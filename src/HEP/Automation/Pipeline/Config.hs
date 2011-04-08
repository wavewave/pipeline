module HEP.Automation.Pipeline.Config where

import Text.Parsec 
import HEP.Automation.MadGraph.Cluster
import HEP.Automation.MadGraph.SetupType

type ConfigParsec = Parsec String ()

config :: ConfigParsec (ScriptSetup,ClusterSetup) 
config = do 
  scriptbase <- p_scriptbase  
  mg5base    <- p_mg5base 
  workbase   <- p_workbase
  cluster    <- p_cluster 
  return (SS scriptbase mg5base workbase, CS cluster)

p_scriptbase :: ConfigParsec String
p_scriptbase = do 
  string "scriptbase"  
  spaces
  char '=' 
  spaces
  str <- many1 (noneOf " \n")
  char '\n'
  return str

p_mg5base :: ConfigParsec String
p_mg5base = do
  string "mg5base"  
  spaces
  char '=' 
  spaces
  str <- many1 (noneOf " \n")
  char '\n'
  return str

p_workbase :: ConfigParsec String
p_workbase = do
  string "workbase"  
  spaces
  char '=' 
  spaces
  str <- many1 (noneOf " \n")
  char '\n'
  return str

p_cluster :: ConfigParsec ClusterRunType
p_cluster = do 
  string "cluster"
  spaces 
  char '='
  spaces 
  numstr <- many1 (digit)
  char '\n'
  let n = read numstr
  if n == 0 
    then return NoParallel 
    else return (Parallel n)