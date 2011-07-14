(* ::Package:: *)

passedevtsFile = "$passedevtsFile$" ;(* file with postfix _passedevts.dat *)
logFile = "$logFile$" ;(* pythia log file, ending _pythia.log *)
binnedPassedEvtsFile = "$binnedPassedEvtsFile$" ;(* use postfix _binned _passedevts.dat *)


Get["Chameleon1_02.m"]


Get["general_functions.m"]


Get["tev_afb_recodataprocessing.m"]


exportRecoBinData[passedevtsFile,logFile][binnedPassedEvtsFile]
