(* ::Package:: *)

pgsfilename = "$pgsfilename$";  (* _pgs_events.lhco.gz *)
pythialogname = "$pythialogname$"; (* _pythia.log *)

cutexportfilename = "$cutexportfilename$"; (* _cut_pgs_events.dat *)

ordevtexportfilename = "$ordevtexportfilename$";  (* _ordered_pgs_events.dat *)
(* must end in .dat. Please use the convention ...setN_ordered _pgs _events.dat for the filename *)

binexportfilename = "$binexportfilename$"; (* _binned_pgs_events.dat. Please use the convention ...setN_binned _pgs _events.dat for the filename *)

njet20exportfilename = "$njetTwoZeroexportfilename$"; (* _njet20_pgs_events.dat *)

njet25exportfilename = "$njetTwoFiveexportfilename$"; (* _njet25_pgs_events.dat *)

  njet30exportfilename = "$njetThreeZeroexportfilename$"; (* _njet30_pgs_events.dat *)


mttbins =Join[ Table[n,{n,0,2000,100}],{2500,\\[Infinity]}];
etabins = Join[Table[n,{n,0,2.4,0.4}],{2.8,\\[Infinity]}];


(* ::Text:: *)
(*It's important to load atlas_lhco_processing_functions.m after Chameleon because in atlas_... the definition of a jet is changed to include Tau Leptons.*)


Get["Chameleon1_02.m"]


Get["general_functions.m"]


Get["atlas_lhco_processing_functions.m"]


(* ::Input:: *)
(*(* took 51 secs to evaluate one file on Moira's computer *)*)


exportCutEvtsOrderedEvtsNJetsAndBinning[pgsfilename,pythialogname][cutexportfilename,ordevtexportfilename,njet20exportfilename,njet25exportfilename,njet30exportfilename,binexportfilename][mttbins,etabins]
