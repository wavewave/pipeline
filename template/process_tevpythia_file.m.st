(* ::Package:: *)

pythiafilename = "$pythiafilename$";    (* "XX_pythia_events.lhe.gz"; *)
pythialogname = "$pythialogname$";  (* "XX_pythia.log"; *)
exportfilename = "$exportfilename$"; (* "XX_binned_pythia_events.dat"; *) (* must end in .dat. Please use the convention ...setN_binned _pythia _events.dat for the filename *)


Get["general_functions.m"]


Get["tev_afb_recodataprocessing.m"]


(* ::Input:: *)
exportPythiaBinData[pythiafilename,pythialogname][exportfilename]
