(* ::Package:: *)

passedevtsFile = "$passedevtsFile$" ;(* file with postfix _passedevts.dat *)
logFile = "$logFile$" ;(* pythia log file, ending _pythia.log *)
binnedPassedEvtsFile = "$binnedPassedEvtsFile$" ;(* use postfix _binned _passedevts.dat *)


Get[chameleondir<>"Chameleon1_02.m"]


Get[workingdir<>"general_functions.m"]


Get[workingdir<>"tev_afb_recodataprocessing.m"]


exportRecoBinData[passedevtsFile,logFile][binnedPassedEvtsFile]
