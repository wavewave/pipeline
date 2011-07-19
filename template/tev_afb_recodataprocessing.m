(* ::Package:: *)

(* ::Text:: *)
(*NOTE: getWeightAndNumEvtsTXT assumes a positive exponent. Should change.*)


(* ::Text:: *)
(*Make sure*)
(*	general_functions.nb*)
(*	Chameleon*)
(*	*)
(*are loaded*)


(* ::Section:: *)
(*extract info from passed events*)


(* ::Subsection:: *)
(*format info*)


lhcoPrtclFormat = {"#","typ","eta","phi","pt","jmas","ntrk","btag","had/em","dum1","dum2"};


recoTopsReturned = {{{"\!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\)","neutrino 4-vector"},{"W jet 1", "W jet 2", "jet 3 of hadronic top","leptonic jet","lepton"}}, "list of extra jets", "event #"};


(* ::Subsection:: *)
(*other*)


matchNegLepEvts[recoEvt_]:=MatchQ[recoEvt,{{{_,{_,_,_,_}},{_List,_List,_List,_List,oLeptonMinus}},_List,_}]
matchPosLepEvts[recoEvt_]:=MatchQ[recoEvt,{{{_,{_,_,_,_}},{_List,_List,_List,_List,oLeptonPlus}},_List,_}]


chiSq[recoEvt_]:=recoEvt[[1,1,1]]


(* ::Subsection:: *)
(*tops*)


lepTopFourVec[recoEvt_]:=recoEvt[[1,1,2]]+(Plus@@(fourVector/@Take[recoEvt[[1,2]],{4,5}])) 


(* ::Text:: *)
(*four - vector (E, px, py, pz) of hadronic top*)


hadTopFourVec[recoEvt_]:=(Plus@@(fourVector/@Take[recoEvt[[1,2]],{1,3}])) 


(* ::Text:: *)
(*four vector of anti-top*)


antiTopFourVec[recoEvt_]:= If[matchNegLepEvts[recoEvt],lepTopFourVec[recoEvt],hadTopFourVec[recoEvt]]


(* ::Text:: *)
(*four vector of top*)


topFourVec[recoEvt_]:= If[matchNegLepEvts[recoEvt],hadTopFourVec[recoEvt],lepTopFourVec[recoEvt]]


(* ::Subsection:: *)
(*cuts*)


getNegLepEvts[recoEvts_]:=Select[recoEvts,matchNegLepEvts]
getPosLepEvts[recoEvts_]:=Select[recoEvts,matchPosLepEvts]


getChiCutEvts[chicut_][recoEvts_]:=Select[recoEvts,chiSq[#]<= chicut&]


(* ::Text:: *)
(*This applies to our reconstructed data because the tops were reconstructed using the 4 hardest jets.*)


get4jetEvts[recoData_]:=Select[recoData,numOf[{_,4,eta_,phi_,pt_,_,_,_,_,_,_}/;Abs[eta]<=2&&pt>=20][#[[2]]]==0&]


get5jetPlusEvts[recoData_]:=Select[recoData,numOf[{_,4,eta_,phi_,pt_,_,_,_,_,_,_}/;Abs[eta]<=2&&pt>=20][#[[2]]]>0&]


(* ::Section:: *)
(*passed event functions*)


(* ::Text:: *)
(*Input is a reconstructed event:*)


yymDAT[evt_]:=Module[{vt,vtbar},

vt=topFourVec[evt];
vtbar = antiTopFourVec[evt];

{rap[vt],rap[vtbar],fourLength[vt+vtbar]}

]



(* ::Section:: *)
(*lhe function*)


yymLHE[evt_]:=Module[{top=Flatten[Cases[evt,oTop]],atop = Flatten[Cases[evt,oATop]],vt,vtbar},If[Length[top]>13,Print["Error: More than one top in this event."]; Abort[];,

vt=fourVector[top];
vtbar = fourVector[atop];

{rap[vt],rap[vtbar],fourLength[vt+vtbar]}

]]


(* ::Section:: *)
(*bin count functions*)


ym[{y_,yb_,m_}]:={y,m};
deltaym[{y_,yb_,m_}]:={y-yb,m};
getm[{y_,yb_,m_}]:=m


labFrameBinData[massbins_][yyms_]:=BinCounts[ym/@yyms,{{-\[Infinity],0,\[Infinity]}},{massbins}];
CMFrameBinData[deltaybins_,massbins_][yyms_]:=BinCounts[deltaym/@yyms,{deltaybins},{massbins}];
\[Sigma]BinData[massbins_][yyms_]:=BinCounts[getm/@yyms,{massbins}]


(* ::Section:: *)
(*binning*)


(* ::Subsection:: *)
(*bin definitions*)


mbinsCDF1 = {350,400,450,500,550,600,700,\[Infinity]};
mbinsCDF1var = {0,350,400,450,500,550,600,700,\[Infinity]};
mbinsCDF2 = {0,450,\[Infinity]};
mbinsfine = Table[x,{x,350,950,1}];
mbinsfine2 = Join[Table[x,{x,345,950,1}],{\[Infinity]}];
mbinsLeps = {0,50,100,150,200,250,300,\[Infinity]};
\[Sigma]mbins = {375-25,375+25,425+25,475+25,525+25,575+25,650+50,750+50,1100+300};
\[Sigma]mbins2 = Join[Table[n,{n,330,900,15}],{\[Infinity]}];


ybins1 = {-\[Infinity],0,\[Infinity]};
ybins2 = {-\[Infinity],-1,0,1,\[Infinity]};


chibinsCDF = Join[Table[n,{n,0,90,10}],{\[Infinity]}];
chibins3 = Join[Table[n,{n,0,30,3}],Table[n,{n,40,90,10}],{\[Infinity]}];


(* ::Subsection:: *)
(*export function*)


exportRecoBinData[datfilename_,logfilename_][exportname_]:=Module[{data,log,wn,yyms,passevts,dN,dNlab,dNcm,chis,dChi3,dChiCDF},
(* import data *)
data =fixImportedExpression/@Import[datfilename];
log = Import[logfilename];

chis = #[[1,1,1]]&/@data;

(* weight and num evts from log file *)
wn = getWeightAndNumEvtsFromLogFile[log];

yyms = yymDAT/@data;

passevts= Length[yyms];

(* bin data *)
dN = \[Sigma]BinData[\[Sigma]mbins2][yyms];

dNlab = labFrameBinData[mbinsCDF2][yyms];

dNcm = CMFrameBinData[ybins1,mbinsCDF2][yyms];

dChi3 = BinCounts[chis,{chibins3}];

dChiCDF = BinCounts[chis,{chibinsCDF}];

(* export data *)
Export[exportname,{wn,{passevts},{dN,dNlab,dNcm},{dChi3,dChiCDF}}];
Remove[data,log,wn,yyms,dN,dNlab,dNcm,chis,dChi3,dChiCDF]
]


exportRecoBinDataForm = {{"weight (pb)","N generated evts"},{"N passed evts"},{"dN \[Sigma]mbins2","dNlab w mbinsCDF2","dNcm w ybins1 mbinsCDF2"},{"dChi3 chibins3","dChiCDF chibinsCDF"}};


exportPythiaBinData[pythiafilename_,logfilename_][exportname_]:=Module[{data,log,wn,yyms,passevts,dN,dNlab,dNcm},
(* import data *)
data =getLHEdata[pythiafilename];
log = Import[logfilename];

(* weight and num evts from log file *)
wn = getWeightAndNumEvtsFromLogFile[log];

yyms = yymLHE/@data;

passevts= Length[yyms];

(* bin data *)
dN = \[Sigma]BinData[\[Sigma]mbins2][yyms];

dNlab = labFrameBinData[mbinsCDF2][yyms];

dNcm = CMFrameBinData[ybins1,mbinsCDF2][yyms];

(* export data *)
Export[exportname,{wn,{passevts},{dN,dNlab,dNcm}}];
Remove[data,log,wn,yyms,dN,dNlab,dNcm]
]


exportPythiaBinDataForm = {{"weight (pb)","N generated evts"},{"N passed evts"},{"dN \[Sigma]mbins2","dNlab w mbinsCDF2","dNcm w ybins1 mbinsCDF2"},{"dChi3 chibins3","dChiCDF chibinsCDF"}};
