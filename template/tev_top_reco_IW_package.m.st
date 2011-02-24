(* ::Package:: *)

(* ::Title:: *)
(*Top Reconstruction at the Tevatron*)


(* ::Subsubtitle:: *)
(*Moira, Feb. 9*)


(* ::Text:: *)
(*Functions in this notebook should be used in conjuction with functions defined in the Chameleon Mathematica package.*)


(* ::Text:: *)
(*Log: *)
(*Changed KSubsets[] to Subsets[] in main reconstruction function. Argument "2" in (K)Subsets changed to "{2}". This was necessary for compatibility with Mathematica Version 8.0.*)


(* ::Section:: *)
(*set directory, load function Chameleon, define physical constants---needs modification by IW.*)


(* ::Text:: *)
(*This should change to the name of the directory where you'll place the Chameleon package:*)


SetDirectory["$chameleondir$"];
<<"Combinatorica`" (* this package is obsolete in version 8, but not in version 7. *)
<<"$chameleon$" 


(* ::Text:: *)
(*These parameters should be the same parameters found in param_card.dat:*)


topMass = 174.3;
wMass = 79.825;
topWidth = 1.51;
wWidth = 2.05;


(* ::Text:: *)
(*Define file names:*)


lhcoFile = "$lhcofile$" ;(* should be the file name/location. e.g. "~/directory/name.lhco" *)
cutEventsFileName = "$sampleCutEvtsdat$"; (* file extension must be .dat *)
recoEventsFileName = "$sampleRecoEvtsdat$";(* file extension must be .dat *)
passedEventsFileName = "$samplePassedEvtsdat$"; (* file extension must be .dat *)
recoInfoFileName = "$sampleRecoInfodat$"; (* file extension must be .dat *)


(* ::Section:: *)
(*load function definitions*)


DropComments[rawObjList_]:= Cases[rawObjList,x_List/; Length[x]>1]; (* keep only objects of length greater than one... which should be only the lists of events, not the comments *)


bJetContent[evt_]:=MatchQ[evt,{___,oB, ___}]||MatchQ[evt,{___,oB}]


oMuEl = {_,1,_,_,_,_,_,__}|{_,2,_,_,_,_,_,__} ;


typOf[{_,typ_,eta_,phi_,pt_,___}]:=typ


MassiveFourVectorFrom[{_,_,\\[Eta]_,\\[Phi]_,pt_,jmas_,___}] := {Sqrt[jmas^2+pt^2 Cosh[\\[Eta]]^2],pt Cos[\\[Phi]],pt Sin[\\[Phi]],pt Sinh[\\[Eta]]}


FourLengthSq[{pe_,pz_,px_,py_}]:=pe^2-pz^2-px^2-py^2


(* lhcoPrtclFormat = {"#","typ","eta","phi","pt","jmas","ntrk","btag","had/em","dum1","dum2"}; *)


jmasOf[{_,_,eta_,phi_,pt_,jmas_,_,___}]:=jmas


(* ::Section:: *)
(*smearing and error*)


(* ::Subsection:: *)
(*error functions*)


(* ::Text:: *)
(*Take the following smearing parameters *)
(**)
(*	Electrons and Muons assuming |\\[Eta]| < 1.1 and Subscript[p, T] > ?? (pg 5, PRD 71, 052003) for "high momentum" track:*)
(*	\\[Delta]pT/pT = 0.001 pT*)
(*	Assume angular resolution is essentially zero.*)
(*	\\[Delta]\\[Phi] = 0 *)
(*	\\[Delta]\\[Theta] = 0*)
(*	*)
(**)
(*	Jets assuming  |\\[Eta]| < 1.1 and 35 < Subscript[E, T] < 450 (end of pg. 5, PRD 71, 052003):*)
(*	Subscript[\\[Delta]E, T] = 0.1 Subscript[E, T] + 1 GeV*)
(*	*)
(*	From D0 estimate in (http://pos.sissa.it/archive/conferences/106/113/DIS%202010_113.pdf, top of pg 3)*)
(*	\\[Delta]\\[Phi] = 0.02*)
(*	\\[Delta]\\[Theta] = 0.02*)


(* ::Text:: *)
(*Energy error function.*)


(* ::Text:: *)
(*The following function takes particle type, \\[Eta], Subscript[p, T] of an object and returns a list of the form { \\[Delta]\\[Eta], \\[Delta]\\[Phi], Subscript[\\[Delta]p, T]}*)


(* ::Text:: *)
(*We'll assume that events are cut so that no jets have |\\[Eta]| > 3 and no leptons have |\\[Eta]| > 2.5. Returns the list {Subscript[\\[Sigma], \\[Eta]], Subscript[\\[Sigma], \\[Phi]],Subscript[\\[Sigma], Subscript[p, T]]}*)


(* ::Text:: *)
(*Note that lepton polar angle smearing is in terms of \\[Theta], not \\[Eta]. And since Tanh \\[Eta] = Cos \\[Theta],*)
(*	Sqrt[1 - Tanh^2 Subscript[\\[Eta], 0]] \\[Delta]\\[Eta] = \\[Delta]\\[Theta]  or  \\[Delta]\\[Eta] = 1/(Sin Subscript[\\[Theta], 0]) \\[Delta]\\[Theta]*)


sigma[typ_,eta_,pt_]:=If[typ==4,(*If[Abs[eta]<3,*){0.02/Sqrt[1 - Tanh[eta]^2],0.02,0.1 pt + 1}(*,{0.02,0.01,\\[Delta]ET[{4.8,0.89,0.043,0}][pt]}]*),If[typ==1||typ==2,{0,0,0.001 pt^2},{0,0,0}]]


(* ::Subsection:: *)
(*ptmiss functions*)


(* ::Text:: *)
(*LHEtoLHCO keeps the eta information of the neutrino. If we want to simulate smearing, we should throw out the eta information and calculate missing pT just like pythia/pgs do---by suming all transverse quantities and seeing what's missing.*)


(* ::Text:: *)
(*These functions are handy*)


pxof[{_,_,_,phi_,pt_,___}]:=pt Cos[phi];
pyof[{_,_,_,phi_,pt_,___}]:=pt Sin[phi];


(* ::Text:: *)
(*The following calculates ptmiss for an event. It returns the event with the missing pt object replaced by a ptmiss object calculated from all visible particle Subscript[p, T]s in the event. The dummy variables in the ptmiss particle are pxmiss and pymiss.*)


(* ::Section:: *)
(*simple cutting functions*)


(* ::Text:: *)
(*Pre-cut for semi-leptonic events with b-tagged jets and photon veto:*)


makeSemiLepCuts[data_]:= Select[data,(NumOf[oL][#]<2)&&(NumOf[oMuEl][#]==1)&&(NumOf[oBJet][#]>0)&&(NumOf[oPhoton][#]<1)&]


(* ::Text:: *)
(*Pt and \\[Eta] cuts (still includes photon veto)*)


makeTightHardCutsTruth[lep\\[Eta]_,lepPt_,met_,noTightJets_,tightJetPt_,tightJet\\[Eta]_,centralB\\[Eta]_]:=(NumOf[{_,6,_,_,pt_,___}/;Abs[pt]>=met][#]===1&&NumOf[{_,4,\\[Eta]_,_,pt_,___}/;And[Abs[\\[Eta]]<tightJet\\[Eta],Abs[pt]>tightJetPt]][#]>= noTightJets&&NumOf[{_,typ_,\\[Eta]_,_,pt_,___}/;And[typ==1||typ==2,Abs[\\[Eta]]<lep\\[Eta],Abs[pt]>lepPt]][#]===1&&NumOf[{_,4,\\[Eta]_,_,_,_,_,btag_,___}/;And[Abs[\\[Eta]]<centralB\\[Eta],btag>0]][#]>0&&NumOf[oPhoton][#]<1)&


makeTightHardCuts[lep\\[Eta]_,lepPt_,met_,noTightJets_,tightJetPt_,tightJet\\[Eta]_,centralB\\[Eta]_][data_]:=Select[data,makeTightHardCutsTruth[lep\\[Eta],lepPt,met,noTightJets,tightJetPt,tightJet\\[Eta],centralB\\[Eta]]]


(* ::Section:: *)
(*covariance matrices and related functions*)


(* ::Text:: *)
(*Covariance matrix \\[LeftAngleBracket]Subscript[p, \\[Mu]] Subscript[p, \\[Nu]]\\[RightAngleBracket] for a single particle species.*)


(* ::Text:: *)
(*Note that \\[LeftAngleBracket]Subscript[p, \\[Mu]] Subscript[p, \\[Nu]]\\[RightAngleBracket] = Subscript[\\[CapitalSigma], i](\\!\\( *)
(*\\*SubscriptBox[\\(\\[PartialD]\\), *)
(*SubscriptBox[\\(q\\), \\(i\\)]]*)
(*\\*SubscriptBox[\\(p\\), \\(\\[Mu]\\)]\\)) (\\[Delta] Subscript[q, i])^2 (\\!\\( *)
(*\\*SubscriptBox[\\(\\[PartialD]\\), *)
(*SubscriptBox[\\(q\\), \\(i\\)]]*)
(*\\*SubscriptBox[\\(p\\), \\(\\[Nu]\\)]\\))  where   q = { Subscript[E, T],\\[Eta],\\[Phi], jmas} for the massive case (covarMassive[]) and  q = { Subscript[E, T],\\[Eta],\\[Phi]} in the massless case (covar[]).*)


pvec[{\\[Eta]_,\\[Phi]_,eT_,m_}]:={Sqrt[m^2+eT^2 Cosh[\\[Eta]]^2],eT Cos[\\[Phi]],eT Sin[\\[Phi]],eT Sinh[\\[Eta]]}


dpvec[{\\[Eta]_,\\[Phi]_,eT_,m_}]:={{(eT^2 Cosh[\\[Eta]] Sinh[\\[Eta]])/Sqrt[m^2+eT^2 Cosh[\\[Eta]]^2],0,0,eT Cosh[\\[Eta]]},{0,-eT Sin[\\[Phi]],eT Cos[\\[Phi]],0},{(eT Cosh[\\[Eta]]^2)/Sqrt[m^2+eT^2 Cosh[\\[Eta]]^2],Cos[\\[Phi]],Sin[\\[Phi]],Sinh[\\[Eta]]}};


covar[{\\[Delta]\\[Eta]_,\\[Delta]\\[Phi]_,\\[Delta]eT_}][{\\[Eta]_,\\[Phi]_,eT_,m_}]=Transpose[dpvec[{\\[Eta],\\[Phi],eT,m}]].(DiagonalMatrix[{\\[Delta]\\[Eta]^2,\\[Delta]\\[Phi]^2,\\[Delta]eT^2}].dpvec[{\\[Eta],\\[Phi],eT,m}])//Simplify;


(* ::Text:: *)
(*Function that sums the covariance matrices \\[LeftAngleBracket]Subscript[p, \\[Mu]] Subscript[p, \\[Nu]]\\[RightAngleBracket] of particles in the list, listOfPrtcls.*)


covarP[prtcl_]:=covar[sigma[typOf[prtcl],etaOf[prtcl],ptOf[prtcl]]][Take[prtcl,{3,6}]]


extraCovar[listOfPrtcls_]:=Plus@@((covar[sigma[typOf[#],etaOf[#],ptOf[#]]][Take[#,{3,6}]]&)/@listOfPrtcls)


(* ::Subsection:: *)
(*dymatrix*)


(* ::Text:: *)
(*Matrix d/(d Subscript[x, i]) Subscript[y, j] where y = {(Subscript[p, W jet 1]+Subscript[p, W jet 2])^2-Subscript[m, W]^2,(Subscript[p, W jet 1]+Subscript[p, W jet 2]+Subscript[p, t jet])^2-Subscript[m, top]^2,Subscript[p, \\[Nu]]^2,(Subscript[p, \\[Nu]]+Subscript[p, lep])^2-Subscript[m, W]^2,(Subscript[p, \\[Nu]]+Subscript[p, lep]+Subscript[p, jet])^2-Subscript[m, top]^2} and Subscript[x, j] are "measured" quantities---including measured momentum components, in addition to the masses Subscript[m, W], Subscript[m, t], and Subscript[m, \\[Nu]].  The masses are sometimes called nuisance parameters. *)


(* ::Text:: *)
(*Here x  = {pJwA0,pJwA1,pJwA2,pJwA3,pJwB0,pJwB1,pJwB2,pJwB3,pJtC0,pJtC1,pJtC2,pJtC3,pJlep0,pJlep1,pJlep2,pJlep3,pL0,pL1,pL2,pL3,p\\[Nu]1,p\\[Nu]2,mW,mT}. The matrix also depends on the 0 and z component of the neutrino (missing energy) momentum four-vector. These will eventually be set to the values for which \\[Chi]^2 is minimized.*)


vec[name_]:=Table[ToExpression[ToString[name]<>ToString[i]],{i,0,3}];


xvec = Join[vec[pJwA],vec[pJwB],vec[pJtC],vec[pJlep],vec[pL],Take[vec[p\\[Nu]],{2,3}],{mW,mT}];


dyMatrix[mW_,mT_][{pJwA0_,pJwA1_,pJwA2_,pJwA3_},{pJwB0_,pJwB1_,pJwB2_,pJwB3_},{pJtC0_,pJtC1_,pJtC2_,pJtC3_},{pJlep0_,pJlep1_,pJlep2_,pJlep3_},{pL0_,pL1_,pL2_,pL3_},{p\\[Nu]1_,p\\[Nu]2_}][p\\[Nu]0_,p\\[Nu]3_] = Transpose[Outer[D[#1,#2]&,{FourLengthSq[vec[pJwA]+vec[pJwB]]-mW^2,FourLengthSq[vec[pJwA]+vec[pJwB]+vec[pJtC]]-mT^2,FourLengthSq[vec[p\\[Nu]]],FourLengthSq[vec[p\\[Nu]]+vec[pL]]-mW^2,FourLengthSq[vec[p\\[Nu]]+vec[pL]+vec[pJlep]]-mT^2},xvec]];


(* ::Subsection:: *)
(*24 x 24 covariance matrix*)


(* ::Text:: *)
(*24 x 24 matrix \\[LeftAngleBracket]Subscript[x, i] Subscript[x, j]\\[RightAngleBracket] - Massive case, includes top and w mass errors*)


uU[\\[Delta]mw_,\\[Delta]mt_][visPrtcls_][extraPrtcls_]:=Module[{numvis,mlength,matrix,missVis,diagMiss,diagMwMt},
numvis = Length[visPrtcls];
(* size of matrix is 4 x num of visible particles + 2 (missing eT) + 2 (mW, mT) *)
mlength = 4*numvis+4;

(* construct block diagonal matrix for visible particle covariance *)
matrix =Plus@@Table[ ArrayPad[covarP[visPrtcls[[i]]],{(i-1)*4,mlength-4 (i)}],{i,1,numvis}];

(* matrix with off-diagonal components from ptmiss-vis particle correlations *)
missVis =PadRight[PadRight[PadLeft[#,mlength-2],mlength]&/@Flatten[ Table[ Take[covarP[visPrtcls[[i]]][[j]],{2,3}],{i,1,numvis},{j,1,4}],1],mlength,{PadRight[{},mlength]}];
missVis = missVis+Transpose[missVis];

(* 2 x 2 ptmiss-ptmiss correlation *)
diagMiss = ArrayPad[Take[#,{2,3}]&/@Take[extraCovar[Join[visPrtcls,extraPrtcls]],{2,3}],{mlength-4,2}];

(* W and top mass correlation *)
diagMwMt = ArrayPad[{{\\[Delta]mw^2,0},{0,\\[Delta]mt^2}},{mlength-2,0}];

(* sum all matrices in the appropriate way*)
matrix - missVis+diagMiss+diagMwMt
]


(* ::Section:: *)
(*maximum likelihood*)


(* ::Text:: *)
(*Main reference: Ian-Woo's appendix in 1008.0405.*)


(* ::Subsection:: *)
(*MAIN MINIMIZATION FUNCTION - uses Simultated Annealing*)


(* ::Text:: *)
(*Uses SimulatedAnnealing. Other functions that utilize other methods are defined below. This method misses the global minimum about 6% of the time.*)


(* ::Text:: *)
(*"SA" is for Simulated Annealing*)


\\[Chi]SqMinSA[mW_,mT_][\\[Delta]mW_,\\[Delta]mT_][fourJetsAndLepton_][pTmiss_List][remainingVisPrtcls_]:=Module[{pL,pJwA,pJwB,pJtC,pJlep,pTnu,\\[Phi]Nu,p\\[Nu], p0Nu,pzNu,yvec, vV24,vV5,min},
(* Interrupt evaluation if the length of fourJetsAndLepton is not exactly 5. *)
If[Length[fourJetsAndLepton ]!=5,"Wrong number of visible particles input as third argument.",

(* Interrupt evaluation if the fifth particle in the list is not a lepton *)
If[Not[MatchQ[fourJetsAndLepton[[5]],oLepton]],
"Fifth particle in particles list is not a lepton.",

(* form four-vectors of visible particles *)
pL = MassiveFourVectorFrom[fourJetsAndLepton[[5]]];
pJwA=MassiveFourVectorFrom[fourJetsAndLepton[[1]]];
pJwB = MassiveFourVectorFrom[fourJetsAndLepton[[2]]];
pJtC = MassiveFourVectorFrom[fourJetsAndLepton[[3]]];
pJlep = MassiveFourVectorFrom[fourJetsAndLepton[[4]]];

(* and neutrino four-vecor *)
pTnu =ptOf[pTmiss];
\\[Phi]Nu = phiOf[pTmiss];
p\\[Nu] = {p0Nu,pTnu Cos[\\[Phi]Nu],pTnu Sin[\\[Phi]Nu],pzNu};

(* form the hypothesis vector *)
yvec={FourLengthSq[pJwA+pJwB]-mW^2,FourLengthSq[pJwA+pJwB+pJtC]-mT^2,FourLengthSq[p\\[Nu]],FourLengthSq[p\\[Nu]+pL]-mW^2,FourLengthSq[p\\[Nu]+pL+pJlep]-mT^2};

(* evaluate 24 x 24 covariance matrix *)
vV24 = uU[\\[Delta]mW,\\[Delta]mT][fourJetsAndLepton][remainingVisPrtcls];

(* evaluate 5 x 5 covariance matrix *)
vV5 = Collect[Transpose[dyMatrix[mW,mT][pJwA,pJwB,pJtC,pJlep,pL,Take[p\\[Nu],{2,3}]][p0Nu,pzNu]].vV24.dyMatrix[mW,mT][pJwA,pJwB,pJtC,pJlep,pL,Take[p\\[Nu],{2,3}]][p0Nu,pzNu],{p0Nu,pzNu}];

(* minimize \\[Chi]^2 *)
 min =  NMinimize[yvec.(Inverse[vV5].yvec),{p0Nu,pzNu},Method->"SimulatedAnnealing"]; 

(* return {\\[Chi]^2, p\\[Nu] four vector} *)
Re[{min[[1]],p\\[Nu]/.min[[2]]}]

]
]
]


(* ::Subsection:: *)
(*MAIN RECONSTRUCTION FUNCTION*)


(* ::Text:: *)
(*A list of True or False, according to whether x matches any of the patterns in pattern_List.*)


matchList[pattern_List][x_]:=MatchQ[x,#]&/@pattern


(* ::Text:: *)
(*Returns the particles in evt that do not match any of the patterns in notList:*)


extraPrtcls[notList_List][evt_]:=Select[evt,Not[(Or@@matchList[notList][#])]&]


(* ::Text:: *)
(*e.g.*)


(* ::Text:: *)
(*extraPrtcls[{oTrigger, oMissingPT, oBJet, oLepton}][wp200evts[[1]]]*)


(* ::Text:: *)
(*{{1, 4, -2.01186, 4.59765, 39.1786, 0., 1., 0, 0., 0., 0.}, {5, 4, -0.799651, 0.0513785, 22.1003, 1.42, 1., 0, 0., 0., 0.}, {6, 4, -2.67186, 0.669574, 58.1851, 0., 1., 0, 0., 0., 0.}}*)


(* ::Text:: *)
(*I assume that there is *at least* one b-jet.*)


recoTops[\\[Delta]mW_,\\[Delta]mT_][mW_,mT_][jetNum_,bJetNum_,chiCut_][evt_]:=
Module[{evtN,lep,pTmiss,jList,bjList,nonbjList,extra,btuples,nonbtuples,tuples,chiList,min,eJets},
(* event number *)
evtN=Flatten[Cases[evt,oTrigger]][[2]];

(* get the lepton and missing Subscript[E, T] from the event. Require exactly one lepton and also missing Subscript[E, T]. *)
lep =HGet[oMuEl,All][evt];
If[Length[lep]!=1,{False,"number of leptons different from 1"},
lep=lep//Flatten;
pTmiss = HGet[oMissingPT,All][evt];
If[Length[pTmiss]<1,{False,"no missing pt"},
pTmiss = pTmiss//Flatten;

(* get only jetNumber hardest jets, return {False, ...} if there aren't enough jets in the event *)
If[NumOf[oJet][evt]<jetNum,{False,"not enough jets in the event"},

(* create lists of jets *)
jList = HGet[oJet,jetNum][evt]; 
bjList = Cases[jList,oBJet];
nonbjList = DeleteCases[jList,oBJet];

(* Require that there are at least bJetNumber B-jets in the list of hardest jetNumber jets.*)
If[Length[bjList]<bJetNum,
{False,"too few hard B-jets"},

(* Require that there are at least two non-B-jets. (W's can't decay to b's) *)
If[Length[nonbjList]<2,{False,"less than two non b jets"},

(* tuples should have the form {Wjet,Wjet,Bjet,lepBjet}*)
If[Length[bjList]>=2,
(* then require two bs to reconstruct the two tops *)
btuples =Permutations[bjList,{2}] ;
nonbtuples=Subsets[nonbjList,{2}];
tuples = Flatten[Outer[Join[#1,#2]&,nonbtuples,btuples,1],1];,
(* otherwise allow one of the tops to NOT have a b-jet *)
(* first form all length 3 permutations of the nonbjets, modulo the first two entries being the same *)
nonbtuples = DeleteDuplicates[Permutations[nonbjList,{3}],Permute[#1,{2,1,3}]==#2&];
(* now insert the b jet in the last position and in the 2nd to last position *)
tuples = Join[(Insert[#,bjList[[1]],-1]&/@nonbtuples),(Insert[#,bjList[[1]],-2]&/@nonbtuples)];
];

(* append the lepton so that tuples has the form {Wjet,Wjet,Bjet,lepBjet,lep}*)
tuples=Append[#,lep]&/@tuples;

(* find minimum \\[Chi]^2 of all the tuples (using Simulated Annealing), return the list {{\\[Chi]^2,\\[Nu]vec},listOfPrtcls} *)
chiList = {\\[Chi]SqMinSA[mW,mT][\\[Delta]mW,\\[Delta]mT][#][pTmiss][extraPrtcls[Join[#,{oTrigger,pTmiss}]][evt]],#}&/@tuples;

(* take the element with smallest \\[Chi]^2 *)
min=First[Sort[chiList,#1[[1,1]]<#2[[1,1]]&]];

(* implement the \\[Chi]^2 cut *)
If[min[[1,1]]> chiCut,{False,"failed to pass chi sq cut"},

(* hard jets that were not used in reconstruction *)
eJets=extraPrtcls[min[[2]]][evt];

(* return {{{\\[Chi]^2,\\[Nu]vec},listOfPrtcls}, {Wjet,Wjet,Bjet,lepBjet,lep}, event #} *)
{min,eJets,evtN}
]
]
]
]
]
]
]


(* ::Text:: *)
(*Useful for remembering the format of the recoTops[] function:*)


(* ::Section:: *)
(*main routine*)


(* ::Text:: *)
(*Upload file:*)


lhco = DropComments[LoadEvents[lhcoFile]];


(* ::Text:: *)
(*Make semileptonic cuts. (Cut parameters are as described in CDF AFB paper, section II). *)


lhcoCutEvts = makeTightHardCuts[1.0,20,20,4,20,2,1][makeSemiLepCuts[lhco]];


Export[cutEventsFileName,lhcoCutEvts]


(* ::Text:: *)
(*Keep all events with \\[Chi]^2 cut of 32, which corresponds roughly to 5 sigma. Cuts on \\[Chi]^2 can easily be done later. Keep 4 hardest jets.*)


recoTime = Timing[recoEvts=recoTops[wWidth,topWidth][wMass,topMass][4,1,$chisqrcut$]/@lhcoCutEvts;]


Export[recoEventsFileName,recoEvts]


recoEvtsPassed=DeleteCases[recoEvts,{False,___}];


Export[passedEventsFileName,recoEvtsPassed]


Export[recoInfoFileName,{"number input events = "<>ToString[Length[lhco]],"events passing semileptonic cut = "<>ToString[Length[lhcoCutEvts]],"time for reconstruction = "<>ToString[Round[First[recoTime]]],"number passed events = "<>ToString[Length[recoEvtsPassed]]}]
