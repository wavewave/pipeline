(* ::Package:: *)

(* ::Section:: *)
(*main routine*)


(* ::Text:: *)
(*This should change to the name of the directory where you'll place the Chameleon package:*)


<<"Combinatorica`" (* this package is obsolete in version 8, but not in version 7. *)
<<"Chameleon1_02.m" 


<<"tev_top_reco_july8.m"


lhcoFile = "$lhcoFile$" (* _pgs _events.lhco or _pgs _events.lhco.gz *);
cutEventsFileName = "$cutEventsFileName$" (* _CutEvts.dat *);
recoEventsFileName = "$recoEventsFileName$" (* _RecoEvts.dat *);
passedEventsFileName = "$passedEventsFileName$" (* PassedEvts.dat *);
recoInfoFileName = "$recoInfoFileName$" (* RecoInfo.dat *);


doReconstruction[lhcoFile][cutEventsFileName,recoEventsFileName,passedEventsFileName,recoInfoFileName]
