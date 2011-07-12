(* ::Package:: *)

(* ::Section:: *)
(*read data files*)


(* ::Subsection:: *)
(*lhco*)


(* ::Text:: *)
(*Main import function, LoadEvents[] is defined in the Chameleon package. The function below should be used in conjunction with LoadEvents[] like so: DropComments[ LoadEvents[ lhcoFile ] ]*)


DropComments[rawObjList_]:= Cases[rawObjList,x_List/; Length[x]>1]; (* keep only objects of length greater than one... which should be only the lists of events, not the comments *)


(* ::Subsection:: *)
(*lhe*)


groupEvents[data_] :=Module[{list={},sublist={},n=1,elt,datalength=Length[data]},
While[n<datalength+1,
elt=data[[n]];
If[elt==={"<event>"},sublist={};,If[elt==={"</event>"},list=Append[list,sublist];,
sublist=Append[sublist,elt];
]
];
n++;
];
list
]


getLHEdata[file_]:=groupEvents[Import[file,"Table"]]


(* ::Subsection:: *)
(*generic*)


(* ::Text:: *)
(*Remove the extra commas that Mathematica seems to include when you import data. You'd use this by doing, e.g., fixImportedExpression/@Import[file.dat]*)


fixImportedExpression[evt_]:=ToExpression[StringReplace[ToString[evt],",,"->","]]


(* ::Subsection:: *)
(*txt, log files*)


getSciTextSign[x_]:=StringDrop[#,1]&@@StringCases[x,"E"~~_]


getSciTextNum[x_String]:=ToExpression[StringDrop[x,-4]]


getSciTextExponent[x_String]:=ToExpression[StringTake[x,-2]]


convertSciText[x_String]:=Module[{num,exp},
num=getSciTextNum[x];
exp = getSciTextExponent[x];
If[getSciTextSign[x]=="+",
num*10^exp,
num*10^(- exp)
]
]


getWeightAndNumEvtsFromTXT[x_String]:=Module[{s,w,n},
s =StringCases[x,"<MGGenerationInfo>"~~___~~"</MGGenerationInfo>"];
If[Length[s]==1,s = First[s];,Print["Error: More or less than one MG event info in file."];Abort[]];
w = First[StringCases[First[StringCases[s,"Integrated weight (pb)"~~___~~"#  Truncated"]],"."~~___~~"E"~~_~~_~~_]];
w=convertSciText[w];
n = ToExpression[StringDrop[StringDrop[First[StringCases[First[StringCases[s,"#  Number of Events"~~___~~"#  Integrated weight"]],":"~~___~~"#"]],1],-1]];
{w,n}
]


getWeightAndNumEvtsFromLogFile[x_String]:=Module[{s,w,n},
s = StringTake[x,-2000]; (* might need to take more of the file if there are lots of subprocesses *)
w = ToExpression[StringTake[s,-25]];
n= ToExpression[First[ StringTake[StringCases[s,"All included subprocesses    I"~~___~~"User process 1"],{35,52}]]];
{w,n}
]


(* ::Section:: *)
(*object patterns*)


(* ::Subsection:: *)
(*lhco*)


lhcoform = {num_,typ_,eta_,phi_,pt_,jmass_,ntrk_,btag_,hadem_, dummy_,dummy_};
oMuEl = {_,1,_,_,_,_,_,__}|{_,2,_,_,_,_,_,__} ;
oMET = {_,6,_,_,_,_,_,__};


(* ::Text:: *)
(*Most of the relevant patterns are defined in the Chameleon package. e.g.:*)


(* ::Input:: *)
(*?oTau*)
(*?oJet*)


(* ::Subsection:: *)
(*lhe*)


oParent={_,-1,0,0,_,_,_,_,___};


oChild = {_,1,_,_,_,_,_,_,_,___};


oTop={6,_,_,_,_,_,_,_,___};
oATop = {-6,_,_,_,_,_,_,_,___};


oChildJet = {1|-1|2|-2|3|-3|4|-4|5|-5|21,1,_,_,_,_,_,_,_,___};
oChildNonBJet = {1|-1|2|-2|3|-3|4|-4|21,1,_,_,_,_,_,_,_,___};
oChildBJet = {5|-5,1,_,_,_,_,_,_,_,___};
oChildLep = {11|-11|13|-13,1,_,_,_,_,_,_,_,___};
oChildEl = {11|-11,1,_,_,_,_,_,_,_,___};
oChildMu = {13|-13,1,_,_,_,_,_,_,_,___};
oChildTau = {15|-15,1,_,_,_,_,_,_,_,___};
oChildNeutrino = {12|-12|14|-14|16|-16,1,_,_,_,_,_,_,_,___};
oChildPhoton = {22,1,_,_,_,_,_,_,_,___};


oZp = {9000006,_,_,_,_,_,_,_,___};
oZpbar = {-9000006,_,_,_,_,_,_,_,___};


lheEvtInfo = {_,_,_,_,_,_};
lhePrtclForm = {id_,st_,moth1_,moth2_,_,_,px_,py_,pz_,e_,m_,_,_};
lheEvtForm = {{_,_,_,_,_,_},{id_,st_,moth1_,moth2_,_,_,px_,py_,pz_,e_,m_,_,_},___};


(* ::Section:: *)
(*kinematic variables*)


(* ::Subsection:: *)
(*four vector*)


fourLength[{e_,px_,py_,pz_}]:=Sqrt[e^2-px^2-py^2-pz^2]


pT[{e_,px_,py_,pz_}]:= Sqrt[px^2+py^2]


etaOf[{e_,px_,py_,pz_}]:=ArcTanh[pz/Norm[{px,py,pz}]]


phi[{e_,px_,py_,pz_}]:=If[px>= 0,ArcTan[py/px],ArcTan[py/px]+\[Pi]]


(* ::Text:: *)
(*Rapidity:*)


rap[{ee_,px_,py_,pz_}]:=(1/2) Log[(ee+pz)/(ee-pz)] (* Faster than arctanh *)


(* ::Text:: *)
(*Transverse mass:*)


mT[{pT1_,\[Phi]1_},{pT2_,\[Phi]2_}]:=2(pT1 pT2 )(1 - Cos[\[Phi]1 - \[Phi]2])


mT[{e1_,px1_,py1_,pz1_},{e2_,px2_,py2_,pz2_}]:=2(Norm[{px1,py1}] Norm[{px2,py2}] - px1 px2 - py1 py2)


mT[{_,_,_,phi1_,pt1_,_,_,_,_,_,_},{_,_,_,phi2_,pt2_,_,_,_,_,_,_}]:=2 (pt1 pt2) (1-Cos[phi1-phi2])


(* ::Text:: *)
(*Delta R for two vectors, x and y:*)


deltaR[x_,y_]:= Sqrt[(etaOf[x]-etaOf[y])^2+(phi[x]-phi[y])^2]


boost[{e_,px_,py_,pz_}]:=Norm[{px,py,pz}]/e


pZ[{e_,px_,py_,pz_}]:=pz


(* ::Subsection:: *)
(*LHE*)


fourVector[{_,_,_,_,_,_,px_,py_,pz_,e_,m_,_,_}]:={e,px,py,pz}


energyOf[{id_,st_,moth1_,moth2_,_,_,px_,py_,pz_,e_,m_,_,_}]:=e


pT[{id_,st_,moth1_,moth2_,_,_,px_,py_,pz_,e_,m_,_,_}]:= Sqrt[px^2+py^2]


etOf[{id_,st_,moth1_,moth2_,_,_,px_,py_,pz_,e_,m_,_,_}]:= Sqrt[px^2+py^2+m^2]


(* ::Text:: *)
(*Pseudorapidity:*)


etaOf[{id_,st_,moth1_,moth2_,_,_,px_,py_,pz_,e_,m_,_,_}]:=ArcTanh[pz/Norm[{px,py,pz}]]


(* ::Text:: *)
(*Azimuthal angle:*)


phi[{id_,st_,moth1_,moth2_,_,_,px_,py_,pz_,e_,m_,_,_}]:=If[px>= 0,ArcTan[py/px],ArcTan[py/px]+\[Pi]]


massOf[{id_,st_,moth1_,moth2_,_,_,px_,py_,pz_,e_,m_,_,_}]:=m


(* ::Subsection:: *)
(*LHCO*)


fourVector[{num_,typ_,eta_,phi_,pt_,jmass_,ntrk_,btag_,hadem_, dummy_,dummy_}]:={Sqrt[jmass^2+(pt Cosh[eta])^2],pt Cos[phi],pt Sin[phi],pt Sinh[eta]} 


energyOf[{num_,typ_,eta_,phi_,pt_,jmass_,ntrk_,btag_,hadem_, dummy_,dummy_}]:=Sqrt[jmass^2+(pt Cosh[eta])^2] 


pT[{num_,typ_,eta_,phi_,pt_,jmass_,ntrk_,btag_,hadem_, dummy_,dummy_}]:= pt


(* ::Text:: *)
(*Pseudorapidity:*)


etaOf[{num_,typ_,eta_,phi_,pt_,jmass_,ntrk_,btag_,hadem_, dummy_,dummy_}]:=eta


(* ::Text:: *)
(*Azimuthal angle:*)


phi[{num_,typ_,eta_,phi_,pt_,jmass_,ntrk_,btag_,hadem_, dummy_,dummy_}]:=phi


massOf[{num_,typ_,eta_,phi_,pt_,jmass_,ntrk_,btag_,hadem_, dummy_,dummy_}]:=jmass


(* ::Text:: *)
(*transverse energy*)


num[{num_,typ_,eta_,phi_,pt_,jmass_,ntrk_,btag_,hadem_, dummy_,dummy_}]:=num


(* ::Section:: *)
(*plotting*)


binboundaries[values_,binning_]:=Module[{counts},
counts = BinCounts[values,{binning}];
Table[{{binning[[i]],counts[[i]]},{binning[[i+1]],counts[[i]]}},{i,1,Length[binning]-1}]
]


histogramPlot[values_,binning_,options___]:=ListPlot[Flatten[binboundaries[values,binning],1],options,Joined->True,Filling->Bottom]


histogramPlot[values_,{scaling_,binning_},options___]:=ListPlot[MapAt[scaling*#&,#,2]&/@ Flatten[binboundaries[values,binning],1],options,Joined->True,Filling->Bottom]


logHistogramPlot[values_,binning_,options___]:=ListLogPlot[Flatten[binboundaries[values,binning],1],options,Joined->True,Filling->Bottom]


logHistogramPlot[values_,{scaling_,binning_},options___]:=ListLogPlot[MapAt[scaling*#&,#,2]&/@Flatten[binboundaries[values,binning],1],options,Joined->True,Filling->Bottom]


(* ::Section:: *)
(*other*)


numOf[pattern_][event_]:=Count[event,pattern]
