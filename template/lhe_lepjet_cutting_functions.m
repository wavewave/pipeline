(* ::Package:: *)

(* ::Text:: *)
(*Note: Could make all functions generic so format can be either lhe or lhco.*)


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


oZp = {9000006,_,_,_,_,_,_,_,___};
oZpbar = {-9000006,_,_,_,_,_,_,_,___};


evtInfo = {_,_,_,_,_,_};


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
(*cutting functions*)


numOf[pattern_][event_]:=Count[event,pattern]


(* ::Subsection:: *)
(*dijet analysis*)


(* ::Text:: *)
(*The following functions require the patterns/functions defined in the section above.*)


(* ::Text:: *)
(*See 1104.0699 for cuts.*)


(* ::Text:: *)
(*initial cuts:*)


(* ::Text:: *)
(*We require the presence of one electron (muon) candidate with ET	(pT ) > 20 GeV (GeV/c) and |\[Eta]| < 1.0 plus missing transverse energy E\:0338 T > 25 GeV. We require events to have exactly two jets each with ET > 30 GeV and |\[Eta]| < 2.4,...*)


makeTightHardCutsTruthLHE[lep\[Eta]_,lepPt_,met_,noTightJets_,tightJetPt_,tightJet\[Eta]_]:=(numOf[x_/;And[MatchQ[x,oChildLep],Abs[etaOf[x]]<lep\[Eta],Abs[pT[x]]>lepPt]][#]===1&&numOf[x_/;MatchQ[x,oChildNeutrino]&&Abs[pT[x]]>=met][#]==1&&numOf[x_/;And[MatchQ[x,oChildJet],Abs[etaOf[x]]<tightJet\[Eta],Abs[pT[x]]>tightJetPt]][#]== noTightJets&&numOf[oChildPhoton][#]<1)&


(* ::Text:: *)
(*finer cuts:*)


(* ::Text:: *)
(*Jets with an electron or muon in a cone \[Laplace]R = 0.52 around the jet axis are removed.*)
(*... and the dijet system to have pT > 40 GeV/c*)
(*transverse mass of lepton missing ET > 30*)
(*jet eta separation, |\[CapitalDelta]\[Eta]| < 2.5*)
(*missing ET and most energetic jet separation |\[CapitalDelta]\[Phi]| > 0.4*)


makeFineCutsTruth[jetPt_,jet\[Eta]_][evt_]:=Module[{met,lep,jets,jet1,jet2},

met = First[Cases[evt,oChildNeutrino]];

lep = If[numOf[oChildLep][evt]!=1,Print["Wrong number of leptons"];Abort[],First[Cases[evt,oChildLep]]];

(* jets, ordered according to energy *)
jets = Sort[Cases[evt,x_/;And[MatchQ[x,oChildJet],Abs[etaOf[x]]<jet\[Eta],Abs[pT[x]]>jetPt]],energyOf[#1]>energyOf[#2]&];

If[Length[jets]!= 2,Print["Wrong number of jets"];Abort[],jet1=First[jets];jet2= Last[jets]];

{deltaR[jet1,lep]>= 0.52,pT[fourVector[jet1]+fourVector[jet2]]> 40,mT[fourVector[lep],fourVector[met]]>30,Abs[etaOf[jet1]-etaOf[jet2]]< 2.5, Abs[phi[met]-phi[jet1]] > 0.4}

]


dijetMass[evt_]:=Module[{jets},jets = Sort[Cases[evt,x_/;And[MatchQ[x,oChildJet],Abs[etaOf[x]]<2.4,Abs[pT[x]]>30]],energyOf[#1]>energyOf[#2]&];

fourLength[Plus@@(fourVector/@jets)]
]


(* ::Subsection:: *)
(*CDF AFB analysis*)


(* ::Text:: *)
(*tau, photon veto. at least 1 B jet. 1 neutrino. 1 lepton.  :*)


makeSemiLepCutsLHE[data_]:= Select[data,(numOf[oChildTau][#]<1)&&(numOf[oChildLep][#]==1)&&(numOf[oChildBJet][#]>0)&&(numOf[oChildPhoton][#]<1)&&(numOf[oChildNeutrino][#]==1)&]


(* ::Text:: *)
(*require:*)
(*1 neutrino (missing energy) with pT > met*)
(*noTightJets jets with Abs[eta] < tightJet\[Eta] and pT > tightJetPt*)
(*1 lepton with Abs[eta] < lep\[Eta] and pT > lepPt*)
(*at least 1 B jet with Abs[eta] < centralB\[Eta]*)
(*no photons *)


makeTightHardCutsTruthCDFttbar[lep\[Eta]_,lepPt_,met_,noTightJets_,tightJetPt_,tightJet\[Eta]_,centralB\[Eta]_]:=(numOf[x_/;MatchQ[x,oChildNeutrino]&&Abs[pT[x]]>=met][#]==1&&numOf[x_/;And[MatchQ[x,oChildJet],Abs[etaOf[x]]<tightJet\[Eta],Abs[pT[x]]>tightJetPt]][#]>= noTightJets&&numOf[x_/;And[MatchQ[x,oChildLep],Abs[etaOf[x]]<lep\[Eta],Abs[pT[x]]>lepPt]][#]===1&&numOf[x_/;And[MatchQ[x,oChildBJet],Abs[etaOf[x]]<centralB\[Eta]]][#]>0&&numOf[oChildPhoton][#]<1)&


(* ::Text:: *)
(*Use these functions to make cuts on a set of events:*)


(* ::Text:: *)
(*This just does tight hard cuts*)


makeTightHardCutsCDFttbar[lep\[Eta]_,lepPt_,met_,noTightJets_,tightJetPt_,tightJet\[Eta]_,centralB\[Eta]_][data_]:=Select[data,makeTightHardCutsTruthCDFttbar[lep\[Eta],lepPt,met,noTightJets,tightJetPt,tightJet\[Eta],centralB\[Eta]]]


cdflep\[Eta] = 1;
cdfleppt = 20;
cdfmet = 20;
cdfNumJets = 4;
cdfjetpt = 20;
cdfjet\[Eta]=2;
cdfbjet\[Eta]=1;


(* ::Section:: *)
(*top functions*)


extractTopInfoLHE[evt_]:=Module[
{top=Flatten[Cases[evt,oTop]],atop = Flatten[Cases[evt,oATop]],vt,vtbar},
If[Length[top]>13,Print["Error: More than one top in this event."]; Abort[];,

vt=fourVector[top];
vtbar = fourVector[atop];

{{etaOf[vt],rap[vt],pT[vt],vt},{etaOf[vtbar],rap[vtbar],pT[vtbar],vtbar},fourLength[vt+vtbar]}

]]


(* ::Text:: *)
(*Need to find the extra jet (should not be decay product of top or Zp). The "fake" top is reconstructed from the Z' and the extra jet.*)


extractFakeTopInfo[evt_]:=Module[
{top=Flatten[Cases[evt,oTop|oATop]],zp = Flatten[Cases[evt,oZp|oZpbar]],
zppos,toppos,extraJet,vtop,vfake, vt,vtbar},
If[Length[top]>13,Print["Error: More than one top in this event."]; Abort[];,

zppos = First[Flatten[Position[DeleteCases[evt,evtInfo],zp]]];
toppos = First[Flatten[Position[DeleteCases[evt,evtInfo],top]]];

extraJet = Flatten[DeleteCases[Cases[evt,oChildJet],{_,_,zppos|toppos,_,_,_,_,_,___}]];

vtop = fourVector[top];
vfake = fourVector[zp]+fourVector[extraJet];

If[First[top]>0,
vt = vtop; vtbar = vfake;,
vt=vfake; vtbar = vtop;];

{{etaOf[vt],rap[vt],pT[vt],vt},{etaOf[vtbar],rap[vtbar],pT[vtbar],vtbar},fourLength[vt+vtbar]}

]]


deltaRap[{{etat_,rapt_,pTt_,vtt_},{etatb_,raptb_,pTtb_,vttb_},mtt_}]:={mtt,rapt-raptb}


etaTop[{{etat_,rapt_,pTt_,vtt_},{etatb_,raptb_,pTtb_,vttb_},mtt_}]:={mtt,etat}


labFrameBinData[massbins_][topinfo_]:=BinCounts[etaTop/@topinfo,{massbins},{{-\[Infinity],0,\[Infinity]}}];
CMFrameBinData[deltaybins_,massbins_][topinfo_]:=BinCounts[deltaRap/@topinfo,{massbins},{deltaybins}];
\[Sigma]BinData[massbins_][topinfo_]:=BinCounts[Last/@topinfo,{massbins}]


cutInfo[t\[Eta]_,tbar\[Eta]_][topinfo_]:=Cases[topinfo,{{etat_,rapt_,pTt_,vtt_},{etatb_,raptb_,pTtb_,vttb_},mtt_}/;And[Abs[etat]<t\[Eta],Abs[etatb]<tbar\[Eta]]]


(* ::Section:: *)
(*import/export functions*)


(* ::Subsection::Closed:: *)
(*extract couplings - needs update*)


getMassAndCoupFromFileName[filename_]:=Module[
{coup},

coup = StringDrop[StringDrop[#,2],-2]&[First[StringCases[filename,"GT"~~___~~"GB"]]];

{Round[ToExpression[StringDrop[StringDrop[#,1],-2]&[First[StringCases[filename,"M"~~___~~"GT"]]]]],ToExpression[If[StringLength[coup]<4,coup,StringTake[coup,4]]]}

]


(* ::Text:: *)
(*For sextet and triplet*)


getMassAndCoupFromFileNameST[filename_]:=Module[
{coup},

coup = StringDrop[StringDrop[#,1],-StringLength["_ttbar"]]&[First[StringCases[filename,"G"~~___~~"_ttbar"]]];

coup = ToString[Round[ToExpression[coup],0.05]];

{Round[ToExpression[StringDrop[StringDrop[#,1],-1]&[First[StringCases[filename,"M"~~___~~"G"]]]]],ToExpression[If[StringLength[coup]<4,coup,StringTake[coup,4]]]}

]


(* ::Input:: *)
(*getMassAndCoupFromFileNameSTLC[filename_]:=Module[*)
(*{coup},*)
(**)
(*coup = StringDrop[StringDrop[#,1],-StringLength["_ttbar"]]&[First[StringCases[filename,"g"~~___~~"_ttbar"]]];*)
(**)
(*coup = ToString[Round[ToExpression[coup],0.05]];*)
(**)
(*{Round[ToExpression[StringDrop[StringDrop[#,1],-1]&[First[StringCases[filename,"m"~~___~~"g"]]]]],ToExpression[If[StringLength[coup]<4,coup,StringTake[coup,4]]]}*)
(**)
(*]*)


getMassAndCoupFromFileNameAxi[filename_]:=Module[
{coup},

coup = StringDrop[StringDrop[#,2],-StringLength["_ttbar"]]&[First[StringCases[filename,"At"~~___~~"_ttbar"]]];

coup = ToString[Round[ToExpression[coup],0.05]];

{Round[ToExpression[StringDrop[StringDrop[#,1],-2]&[First[StringCases[filename,"M"~~___~~"Vq"]]]]],ToExpression[If[StringLength[coup]<4,coup,StringTake[coup,4]]]}

]


(* ::Input:: *)
(*getMassAndCoupFromFileNameAxi2LC[filename_]:=Module[*)
(*{coups,coupq,coupt},*)
(**)
(*coups = StringDrop[StringDrop[#,2],-StringLength["_ttbar_teva_nomatch_nocut_set1"]]&[First[StringCases[filename,"aq"~~___]]];*)
(**)
(*coupq = StringDrop[First[StringCases[coups,___~~"at"]],-2];*)
(**)
(*coupq = ToString[Round[ToExpression[coupq],0.05]];*)
(**)
(*coupt = StringDrop[First[StringCases[coups,"at"~~___]],2];*)
(**)
(*coupt = ToString[Round[ToExpression[coupt],0.05]];*)
(**)
(**)
(*{Round[ToExpression[StringDrop[StringDrop[#,1],-2]&[First[StringCases[filename,"m"~~___~~"vq"]]]]],ToExpression[If[StringLength[coupq]<4,coupq,StringTake[coupq,4]]],ToExpression[If[StringLength[coupt]<4,coupt,StringTake[coupt,4]]]}*)
(**)
(*]*)


getMassAndCoupFromFileNameLC[filename_]:=Module[
{coup},

coup = StringDrop[StringDrop[#,2],-2]&[First[StringCases[filename,"gt"~~___~~"gb"]]];

{Round[ToExpression[StringDrop[StringDrop[#,1],-2]&[First[StringCases[filename,"m"~~___~~"gt"]]]]],ToExpression[If[StringLength[coup]<4,coup,StringTake[coup,4]]]}

]


(* ::Input:: *)
(* *)


(* ::Subsection:: *)
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
num*10^(-exp) (* corrected Apr 21 *)
]
]


getWeightAndNumEvtsFromTXT[x_String]:=Module[{s,w,n},
s =StringCases[x,"<MGGenerationInfo>"~~___~~"</MGGenerationInfo>"];
If[Length[s]==1,s = First[s];,Print["Error: More or less than one MG event info in file."];Abort[]];
w = StringDrop[First[StringCases[First[StringCases[s,"Integrated weight (pb)"~~___~~"#  Truncated"]],":"~~___~~"E"~~_~~_~~_]],1]; (* corrected Apr 21 *)
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


exportTopInfo[importfile_,bannerfile_][exportfile_]:=Export[exportfile,{getWeightAndNumEvtsFromTXT[Import[bannerfile]],extractTopInfoLHE/@getLHEdata[importfile]}]


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


succinctinfo[{{xsec_,numevts_},topinfo_}]:={xsec,numevts,efffactor[{{xsec,numevts},topinfo}],rawAFBdm[{{xsec,numevts},topinfo}]}


succinctinfo2[{{xsec_,numevts_},topinfo_}]:={xsec,numevts,efffactor[{{xsec,numevts},topinfo}],rawAFBdm[{{xsec,numevts},topinfo}],effAFBdm[1,2][{{xsec,numevts},topinfo}],rawAFBdy[{{xsec,numevts},topinfo}],effAFBdy[1,2][{{xsec,numevts},topinfo}]}


(* ::Section:: *)
(*plotting functions*)


makehist[topinfo_,options___]:=Show[SmoothDensityHistogram[etaTop/@topinfo,ColorFunction->Hue,Frame->True,Axes->False,PlotRange->{{350,1000},{-4,4}},FrameLabel->{"\!\(\*SubscriptBox[\(m\), \(t\\\ \*OverscriptBox[\(t\), \(_\)]\)]\)","\!\(\*SubscriptBox[\(\[Eta]\), \(t\)]\)"},options],Plot[{1,0,-1},{x,350,1000},PlotStyle->Thick]]


(* ::Section:: *)
(*new export functions*)


exportBinAndTopInfo[importfile_,bannerfile_][uncutfilename_,cutfilename_,bincountsfilename_]:=Module[{evts,cutevts,uncuttinfo,cutttinfo,uncutbininfo,cutbininfo,w},

evts = getLHEdata[importfile];
uncuttinfo = extractTopInfoLHE/@evts;

cutevts = makeTightHardCutsCDFttbar[cdflep\[Eta],cdfleppt,cdfmet,cdfNumJets,cdfjetpt,cdfjet\[Eta],cdfbjet\[Eta]][evts];
cutttinfo = extractTopInfoLHE/@cutevts;

uncutbininfo = CMFrameBinData[ybins2,mbins][uncuttinfo];
cutbininfo = CMFrameBinData[ybins2,mbins][cutttinfo];

w=getWeightAndNumEvtsFromTXT[Import[bannerfile]];

Export[uncutfilename,{w,uncuttinfo}];
Export[cutfilename,{w,cutttinfo}];
Export[bincountsfilename,{w,cutbininfo,uncutbininfo}];

Remove[evts,uncuttinfo,cutevts,cutttinfo,uncutbininfo,cutbininfo,w];

]


exportBinAndTopInfoSingleTop[importfile_,bannerfile_][uncutfilename_,cutfilename_,bincountsfilename_]:=Module[{evts,cutevts,uncuttinfo,cutttinfo,uncutbininfo,cutbininfo,w},

evts = getLHEdata[importfile];
uncuttinfo = extractFakeTopInfo/@evts;

cutevts = makeTightHardCutsCDFttbar[cdflep\[Eta],cdfleppt,cdfmet,cdfNumJets,cdfjetpt,cdfjet\[Eta],cdfbjet\[Eta]][evts];
cutttinfo = extractFakeTopInfo/@cutevts;

uncutbininfo = CMFrameBinData[ybins2,mbins][uncuttinfo];
cutbininfo = CMFrameBinData[ybins2,mbins][cutttinfo];

w=getWeightAndNumEvtsFromTXT[Import[bannerfile]];

Export[uncutfilename,{w,uncuttinfo}];
Export[cutfilename,{w,cutttinfo}];
Export[bincountsfilename,{w,cutbininfo,uncutbininfo}];

Remove[evts,uncuttinfo,cutevts,cutttinfo,uncutbininfo,cutbininfo,w];

]
