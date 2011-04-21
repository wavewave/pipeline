(* ::Package:: *)

(* ::Section:: *)
(*object patterns*)


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


(* ::Section:: *)
(*kinematic variables*)


fourVector[{_,_,_,_,_,_,px_,py_,pz_,e_,m_,_,_}]:={e,px,py,pz}


fourLength[{e_,px_,py_,pz_}]:=Sqrt[e^2-px^2-py^2-pz^2]


energyOf[{id_,st_,moth1_,moth2_,_,_,px_,py_,pz_,e_,m_,_,_}]:=e


pT[{id_,st_,moth1_,moth2_,_,_,px_,py_,pz_,e_,m_,_,_}]:= Sqrt[px^2+py^2]


pT[{e_,px_,py_,pz_}]:= Sqrt[px^2+py^2]


(* ::Text:: *)
(*Pseudorapidity:*)


etaOf[{id_,st_,moth1_,moth2_,_,_,px_,py_,pz_,e_,m_,_,_}]:=ArcTanh[pz/Norm[{px,py,pz}]]


etaOf[{e_,px_,py_,pz_}]:=ArcTanh[pz/Norm[{px,py,pz}]]


(* ::Text:: *)
(*Azimuthal angle:*)


phi[{e_,px_,py_,pz_}]:=If[px>= 0,ArcTan[py/px],ArcTan[py/px]+\[Pi]]


phi[{id_,st_,moth1_,moth2_,_,_,px_,py_,pz_,e_,m_,_,_}]:=If[px>= 0,ArcTan[py/px],ArcTan[py/px]+\[Pi]]


(* ::Text:: *)
(*Rapidity:*)


rap[{ee_,px_,py_,pz_}]:=(1/2) Log[(ee+pz)/(ee-pz)] (* Faster than arctanh *)


(* ::Text:: *)
(*Transverse mass:*)


mT[{pT1_,\[Phi]1_},{pT2_,\[Phi]2_}]:=2(pT1 pT2 )(1 - Cos[\[Phi]1 - \[Phi]2])


mT[{e1_,px1_,py1_,pz1_},{e2_,px2_,py2_,pz2_}]:=2(Norm[{px1,py1}] Norm[{px2,py2}] - px1 px2 - py1 py2)


(* ::Text:: *)
(*Delta R for two vectors, x and y:*)


deltaR[x_,y_]:= Sqrt[(etaOf[x]-etaOf[y])^2+(phi[x]-phi[y])^2]


(* ::Section:: *)
(*top functions*)


extractTopInfoLHE[evt_]:=Module[
{top=Flatten[Cases[evt,oTop]],atop = Flatten[Cases[evt,oATop]],vt,vtbar},
If[Length[top]>13,Print["Error: More than one top in this event."]; Abort[];,

vt=fourVector[top];
vtbar = fourVector[atop];

{{etaOf[vt],rap[vt],pT[vt],vt},{etaOf[vtbar],rap[vtbar],pT[vtbar],vtbar},fourLength[vt+vtbar]}

]]


deltaRap[{{etat_,rapt_,pTt_,vtt_},{etatb_,raptb_,pTtb_,vttb_},mtt_}]:={mtt,rapt-raptb}


etaTop[{{etat_,rapt_,pTt_,vtt_},{etatb_,raptb_,pTtb_,vttb_},mtt_}]:={mtt,etat}


labFrameBinData[massbins_][topinfo_]:=BinCounts[etaTop/@topinfo,{massbins},{{-\[Infinity],0,\[Infinity]}}];
CMFrameBinData[deltaybins_,massbins_][topinfo_]:=BinCounts[deltaRap/@topinfo,{massbins},{deltaybins}];
\[Sigma]BinData[massbins_][topinfo_]:=BinCounts[Last/@topinfo,{massbins}]


cutInfo[t\[Eta]_,tbar\[Eta]_][topinfo_]:=Cases[topinfo,{{etat_,rapt_,pTt_,vtt_},{etatb_,raptb_,pTtb_,vttb_},mtt_}/;And[Abs[etat]<t\[Eta],Abs[etatb]<tbar\[Eta]]]


(* ::Section:: *)
(*data files*)


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


(* ::Text:: *)
(*For importing .dat files with arrays:*)


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
num*10^- exp
]
]


getWeightAndNumEvtsFromTXT[x_String]:=Module[{s,w,n},
s =StringCases[x,"<MGGenerationInfo>"~~___~~"</MGGenerationInfo>"];
If[Length[s]==1,s = First[s];,Print["Error: More or less than one MG event info in file."];Abort[]];
w = First[StringCases[First[StringCases[s,"Integrated weight (pb)"~~___~~"#  Truncated"]],"."~~___~~"+"~~_~~_]];
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
(*top info functions*)


passedevts[\[Eta]t_,\[Eta]tbar_][{{xsec_,numevts_},topinfo_}]:=Length[Cases[topinfo,{{etat_,rapt_,pTt_,vtt_},{etatb_,raptb_,pTtb_,vttb_},mtt_}/;And[Abs[etat]<\[Eta]t,Abs[etatb]<\[Eta]tbar]]]


efficiency[\[Eta]t_,\[Eta]tbar_][{{xsec_,numevts_},topinfo_}]:=passedevts[\[Eta]t,\[Eta]tbar][{{xsec,numevts},topinfo}]/numevts//N


smeff1 = 0.529;
smeff2 = 0.5364;


efffactor[{{xsec_,numevts_},topinfo_}]:=1/2 (efficiency[2,1][{{xsec,numevts},topinfo}]/smeff1+efficiency[1,2][{{xsec,numevts},topinfo}]/smeff2)


afb[list_]:= Module[{l,f,b},

If[OddQ[Length[list]],Abort[]];

l=Length[list];

f=Take[list,-l/2];
b=Reverse[Take[list,l/2]];

(f-b)/(f+b)

]


afbpair[list_]:= Module[{l,f,b},

If[OddQ[Length[list]],Abort[]];

l=Length[list];

f=Take[list,-l/2];
b=Reverse[Take[list,l/2]];

{(f-b)/(f+b),f+b}

]


afberror[{afb_,fplusb_}]:=ToString[afb]<>"\[PlusMinus]"<>ToString[Sqrt[1-afb^2]/Sqrt[fplusb]]


ybins = {-\[Infinity],0,\[Infinity]};
ybins2 = {-\[Infinity],-1,0,1,\[Infinity]};
mbins = {0,450,\[Infinity]};


rawAFBdm[{{xsec_,numevts_},topinfo_}]:=Flatten/@(afbpair/@CMFrameBinData[ybins,mbins][topinfo])//N


effAFBdm[eta1_,eta2_][{{xsec_,numevts_},topinfo_}]:=Flatten/@(1/2 (afbpair/@CMFrameBinData[ybins,mbins][cutInfo[eta1,eta2][topinfo]]+afbpair/@CMFrameBinData[ybins,mbins][cutInfo[eta2,eta1][topinfo]]))//N


rawAFBdy[{{xsec_,numevts_},topinfo_}]:=Transpose[First[(afbpair/@CMFrameBinData[ybins2,{0,\[Infinity]}][topinfo])]]//N


effAFBdy[eta1_,eta2_][{{xsec_,numevts_},topinfo_}]:=Transpose[First[(1/2 (afbpair/@CMFrameBinData[ybins2,{0,\[Infinity]}][cutInfo[eta1,eta2][topinfo]]+afbpair/@CMFrameBinData[ybins2,{0,\[Infinity]}][cutInfo[eta2,eta1][topinfo]]))]]//N


succinctinfo[{{xsec_,numevts_},topinfo_}]:={xsec,numevts,efffactor[{{xsec,numevts},topinfo}],rawAFBpair[{{xsec,numevts},topinfo}]}


succinctinfo2[{{xsec_,numevts_},topinfo_}]:={xsec,numevts,efffactor[{{xsec,numevts},topinfo}],rawAFBdm[{{xsec,numevts},topinfo}],effAFBdm[1,2][{{xsec,numevts},topinfo}],rawAFBdy[{{xsec,numevts},topinfo}],effAFBdy[1,2][{{xsec,numevts},topinfo}]}


(* ::Section:: *)
(*export function*)


exportTopInfo[importfile_,bannerfile_][exportfile_]:=Export[exportfile,{getWeightAndNumEvtsFromTXT[Import[bannerfile]],extractTopInfoLHE/@getLHEdata[importfile]}]


exportTopInfo2[importfile_,bannerfile_][exportfile1_,exportfile2_]:=Module[{info},

info = {getWeightAndNumEvtsFromTXT[Import[bannerfile]],extractTopInfoLHE/@getLHEdata[importfile]};

Export[exportfile1,info];
Export[exportfile2,succinctinfo[info]];

Remove[info]
]


exportTopInfo3[importfile_,bannerfile_][exportfile1_,exportfile2_]:=Module[{info},

info = {getWeightAndNumEvtsFromTXT[Import[bannerfile]],extractTopInfoLHE/@getLHEdata[importfile]};

Export[exportfile1,info];
Export[exportfile2,succinctinfo2[info]];

Remove[info]
]
