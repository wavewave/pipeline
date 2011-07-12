(* ::Package:: *)

(* ::Section:: *)
(*generic functions*)


mttpair[f_][evt_]:={mttOf[evt],f[evt]}


pair[f_,g_][evt_]:={f[evt],g[evt]}


mttEtaPair[evt_]:=mttpair[leptonEta][evt]


mttAbsEtaPair[evt_]:=mttpair[Abs[leptonEta[#]]&][evt]


(* ::Section:: *)
(*binning*)


doMttEtaBinning[mttbins_,etabins_][listOfEvents_]:=BinCounts[mttAbsEtaPair/@listOfEvents,{mttbins},{etabins}]


njetsbins = Join[Table[n,{n,-0.5,8.5}],{\[Infinity]}]


doNJetsBinning[ptmin_,etamax_][mttbins_][listOfEvents_]:=BinCounts[mttpair[nExtraJets[ptmin,etamax]]/@listOfEvents,{mttbins},{njetsbins}]


(* ::Title:: *)
(*selection and jet energy correction*)


(* ::Text:: *)
(*Required: Functions in Chameleon package*)


(* ::Text:: *)
(*For this analysis we will treat Tau Leptons as jets. So redfine the jet object:*)


oJet = {_,4,_,_,_,_,_,__}|{_,3,_,_,_,_,_,__};


(* ::Section:: *)
(*energy correction*)


(* ::Text:: *)
(*See jet_smearing_2.nb for derivation of this energy correction for the PGS implementation of the Atlas detector.*)


dPtOverPt[pt_,\[Eta]_,m_]:=(14.23469362115847\[VeryThinSpace]+7.532390890556188 \[Eta]^2)/Sqrt[(pt Cosh[\[Eta]])^2+m^2]


correctedPt[pt_,\[Eta]_,m_]:=pt(1 + dPtOverPt[pt,\[Eta],m])


(* ::Text:: *)
(*Assume: (\[CapitalDelta] E)/E = (\[CapitalDelta] Subscript[p, T])/Subscript[p, T] *)


(* ::Text:: *)
(* E = Sqrt[Subscript[p, T]^2 cosh^2 \[Eta] + m^2] \[LongRightArrow] Subscript[E, new]=Sqrt[(Subscript[p, T](1 + (\[CapitalDelta] Subscript[p, T])/Subscript[p, T]))^2 cosh^2 \[Eta] + Subscript[m, new]^2] = Subscript[E, old](1 + (\[CapitalDelta] Subscript[p, T])/Subscript[p, T]) =Sqrt[Subscript[p, T]^2 cosh^2 \[Eta] + m^2](1 + (\[CapitalDelta] Subscript[p, T])/Subscript[p, T])*)
(**)
(*which implies that Subscript[m, new] = (1 + (\[CapitalDelta] Subscript[p, T])/Subscript[p, T]) Subscript[m, old] *)


correctedM[pt_,\[Eta]_,m_]:=m (1 + dPtOverPt[pt,\[Eta],m])


correctObjectEnergy[{num_,typ_,eta_,phi_,pt_,jmass_,ntrk_,btag_,hadem_,dummy1_,dummy2_}]:={num,typ,eta,phi,correctedPt[pt,eta,jmass],correctedM[pt,eta,jmass],ntrk,btag,hadem,dummy1,dummy2}


correctJetEnergy[x_]:=If[MatchQ[x,oJet],correctObjectEnergy[x],x]


correctEvtJetEnergies[evt_]:=correctJetEnergy/@evt


(* ::Section:: *)
(*coarse-grained selection*)


(* ::Text:: *)
(*Most of these are functions of an event (evt) that return True or False *)


(* ::Text:: *)
(*Lepton Reconstruction:*)
(*e: 	p_T > 25, |\eta_cluster| < 2.47, 1.3 < |\eta| < 1.52 are excluded, (E_T w/in dR=0.2) - E_T_electron < 4 GeV*)
(*\mu: 	p_T > 20, |\eta| < 2.5 , (E_T w/in dR=0.3) - E_T_muon < 2.5 GeV, (\sum |E_T| w/in dR=0.3) - p_T_muon < 2.5, dR_{muon and any jet with p_T > 20} > 0.4*)
(**)
(*Note: It only makes sense to do lepton isolation *before* jets have been recombined. I will make only the Subscript[p, T] and \[Eta] cuts*)


(* ::Text:: *)
(*Require number of isolated muons or electrons to be exactly 1*)
(*Require at least four jets with p_T > 25 and |\eta| < 2.5*)
(*Require at least one of the jets to be b-tagged*)


(* ::Text:: *)
(*Since PGS doesn't have a crack between eta=1.3 and eta=1.52, just set the max eta for electrons to be 2.5.*)


jetPt = 25;
jetEta = 2.5;
nBJets = 1;
nJets = 4;


elPt = 25;
muPt = 20;
elEta = 2.5;
muEta = 2.5;


(* ::Text:: *)
(*Jets:*)
(**)
(*at least four with p_T > 25 and |\eta| < 2.5*)
(*at least one of the jets must be b-tagged*)


njetsSelection[evt_] := NumOf[x_/;And[MatchQ[x,oJet],Abs[etaOf[x]]<jetEta,Abs[pT[x]]>jetPt]][evt]>= nJets


bjetsSelection[evt_] := NumOf[x_/;And[MatchQ[x,oBJet],Abs[etaOf[x]]<jetEta,Abs[pT[x]]>jetPt]][evt]>= nBJets


(* ::Text:: *)
(*SELECTION:*)
(**)
(*Exactly 1 isolated lepton*)


(* ::Text:: *)
(*Lepton Reconstruction:*)
(*e: 	p_T > 25, |\eta_cluster| < 2.47, 1.3 < |\eta| < 1.52 are excluded, (E_T w/in dR=0.2) - E_T_electron < 4 GeV*)
(*\mu: 	p_T > 20, |\eta| < 2.5 , (E_T w/in dR=0.3) - E_T_muon < 2.5 GeV, (\sum |E_T| w/in dR=0.3) - p_T_muon < 2.5, dR_{muon and any jet with p_T > 20} > 0.4*)
(**)
(*Note: It only makes sense to do lepton isolation *before* jets have been recombined. I will make only the Subscript[p, T] and \[Eta] cuts*)


semiLepSelection[evt_] :=(NumOf[oElectron][evt]+NumOf[oMuon][evt]==1)


tightLepSelection[evt_]:=(NumOf[x_/;And[MatchQ[x,oElectron],Abs[etaOf[x]]<elEta,Abs[pT[x]]>elPt ]][evt]+NumOf[x_/;And[MatchQ[x,oMuon],Abs[etaOf[x]]<muEta,Abs[pT[x]]>muPt ]][evt]==1)


(* ::Text:: *)
(*Note: Jet energy correction should be applied *before* applying this selection.*)


makeInitialCuts[eventlist_]:=Select[eventlist,And[semiLepSelection[#],tightLepSelection[#],njetsSelection[#],bjetsSelection[#]]&]


(* ::Section:: *)
(*finer-grained selection*)


(* ::Text:: *)
(*if Electron:*)
(*E_T^miss > 35*)
(*M_T(lepton, E)T^miss) > 25*)
(**)
(*if Muon:*)
(*E_T^miss > 20*)
(*E_T^miss + M_T(lepton, E_T^miss) > 60*)


etElMissPt=35;
etMuMissPt = 20;
mtElMissPt = 25;
mtPlusPtMuMissPt = 60;


electronSelection[evt_]:= 
Module[{emiss,electron},
If[NumOf[oElectron][evt]!=1,Print["Error: non-electron event fed into electronSelection function."];Abort[]] ;

If[NumOf[oMissingPT][evt]!=1,Return[False],emiss = First[Cases[evt,oMissingPT]]];

electron = First[Cases[evt,oElectron]];

And[pT[emiss]>etElMissPt,mT[emiss,electron]>mtElMissPt]

]


muonSelection[evt_]:= 
Module[{emiss,muon},
If[NumOf[oMuon][evt]!=1,Print["Error: non-muon event fed into muonSelection function."];Abort[]] ;

If[NumOf[oMissingPT][evt]!=1,Return[False],emiss = First[Cases[evt,oMissingPT]]];

muon = First[Cases[evt,oMuon]];

And[pT[emiss]>etMuMissPt,pT[emiss]+mT[emiss,muon]>mtPlusPtMuMissPt]

]


leptonSelection[evt_]:=If[NumOf[oMuon][evt]+NumOf[oElectron][evt]!=1,False,If[NumOf[oMuon][evt]==1,muonSelection[evt],electronSelection[evt]]]


makeSecondCuts[eventlist_]:=Select[eventlist,leptonSelection]


(* ::Text:: *)
(**)
(*Other Selection:*)
(**)
(*jets w/in \Delta R < 0.2 of electron candidate are removed (to avoid double-counting of electrons as jets)*)


removeElectronJets[evt_]:=Module[{el},
If[NumOf[oElectron][evt]<1,Return[evt]];
el = Cases[evt,oElectron];
DeleteCases[evt,x_/;(And[MatchQ[x,oJet],#]&@@(deltaR[x,#]<0.2&/@el))]
]


(* ::Section:: *)
(*correct energies and make cuts*)


makeEnergyCorrectionAndCuts[listOfEvents_]:=removeElectronJets/@makeSecondCuts[makeInitialCuts[correctEvtJetEnergies/@listOfEvents]]


(* ::Section:: *)
(*export functions*)


exportCutEvents[lhcoFileName_,exportFileName_]:=If[Not[FileExistsQ[lhcoFileName]],Return["Cannot find file "<>lhcoFileName<>"."],Export[exportFileName,makeEnergyCorrectionAndCuts[DropComments[LoadEvents[lhcoFileName]]]]] (* exportFileName should end in .dat *)


(* ::Title:: *)
(*Subscript[m, t *)
(*\!\(\*OverscriptBox[\(t\), \(_\)]\)] reconstruction*)


(* ::Section:: *)
(*dRmin algorithm for selecting jets*)


jetptmin = 20;
jetetamax = 2.5;


dRmin[jmass_]:=2.5 - 0.015 jmass


getAndOrderJets[evt_]:=Sort[Cases[evt,x_/;And[MatchQ[x,oJet],pT[x]>jetptmin,Abs[etaOf[x]]<jetetamax]],pT[#1]>pT[#2]&]


dRList[jet_,visobjectslist_]:=deltaR[jet,#]&/@visobjectslist


(* ::Text:: *)
(*Returns True if the jet's minimum distance to a particle in visobjectslist is > dRmin*)


dRminTest[jet_,visobjectslist_]:=Min[dRList[jet,visobjectslist]] > dRmin[massOf[jet]]


(* ::Text:: *)
(*Returns the 3 or 4 leading jets after excluding any "far away" jets, following the dRmin algorithm. Note the added test condition in the while loop:  the number of jets must be greater than 4 in order to go through the dRmin algorithm.*)


getLeadingJets[evt_]:=Module[{lep,jets,shortjets,notjetlist,lfar=1,farjets},
lep=First[Cases[evt,oLepton]];
jets = getAndOrderJets[evt];

While[
(* TEST *)
And[lfar>0,Length[jets]> 3],

(* BODY *)
shortjets = If[Length[jets]<4,jets,Take[jets,4]];

notjetlist[jet_] := Join[DeleteCases[shortjets,jet],{lep}];

(* select jets that do no satisfy dRmin criteria *)
farjets = Select[shortjets,dRminTest[#,notjetlist[#]]&];
lfar = Length[farjets];

(* order jets if there is more than one *)
If[lfar>1,farjets = Sort[farjets,Min[dRList[#1,notjetlist[#1]]]>Min[dRList[#2,notjetlist[#2]]]&]];

(* delete the farthest jet from jetlist. if statement is strictly not necessary b/c while loop will jump out if lfar = 0 *)

jets=If[lfar>0,DeleteCases[jets,First[farjets]],jets];
];

If[Length[jets]<4,jets,Take[jets,4]] 

]


(* ::Section:: *)
(*neutrino momentum *)


mW = 80.4;


s\[Eta]solutions[{ptmiss_,\[Phi]_},{eta_,phi_,pt_,jmass_}]:=Module[{s\[Eta],soln},
soln = Solve[fourLength[{ptmiss Sqrt[1+s\[Eta]^2],ptmiss Cos[\[Phi]],ptmiss Sin[\[Phi]],ptmiss s\[Eta]}+ {Sqrt[jmass^2+pt^2 Cosh[eta]^2],pt Cos[phi],pt Sin[phi],pt Sinh[eta]}]==mW,s\[Eta]];
If[soln=={},"No Solution.",s\[Eta]/.soln]];
s\[Eta]solutions[{_,6,_,\[Phi]_,ptmiss_,_,_,_,_,_,_},{_,_,eta_,phi_,pt_,jmass_,_,_,_,_,_}]:=s\[Eta]solutions[{ptmiss,\[Phi]},{eta,phi,pt,jmass}]


s\[Eta]solution[{ptmiss_,\[Phi]_},{eta_,phi_,pt_,jmass_}]:=Module[{s,ss},
ss=s\[Eta]solutions[{ptmiss,\[Phi]},{eta,phi,pt,jmass}];

If[ss=="No Solution.",Return["No Solution."]];
s=Re/@ss;

First[Sort[s,Abs[#1]<Abs[#2]&]]
]
s\[Eta]solution[{_,6,_,\[Phi]_,ptmiss_,_,_,_,_,_,_},{_,_,eta_,phi_,pt_,jmass_,_,_,_,_,_}]:=s\[Eta]solution[{ptmiss,\[Phi]},{eta,phi,pt,jmass}]


(* ::Text:: *)
(*The following two functions assume there's only one lepton and one missing pt in the event.*)


getMET[evt_]:=First[Cases[evt,oMissingPT]]


getLEP[evt_]:=First[Cases[evt,oElectron|oMuon]]


(* ::Section:: *)
(*top decay products*)


(* ::Text:: *)
(*This function returns the list {Subscript[m, t tbar], {lep, {leading jets}, met},{extra jets},{extra jets with pt > 20 and |eta| < 2.5 }} where met includes \[Eta] gotten from W mass constraint and the 4 leading jets are chosen with the dRmin algorithm*)


notMatch[x_][list_]:=Not[Or@@(MatchQ[x,#]&/@list)]


orderParticles[evt_]:=Module[{lep,met,sinh\[Eta],jets,extrajets},
(* define lepton *)
lep=Cases[evt,oElectron|oMuon];
lep=If[Length[lep]!=1,"Error: Number of electrons or muons in event was not equal to 1.";Abort[],
First[lep]];

(* define met *)
met=Cases[evt,oMissingPT];
met=If[Length[met]!=1,"Error: Number of MET in event was not equal to 1.";Abort[],
First[met]];

sinh\[Eta] = s\[Eta]solution[met,lep];

(* define leading jets *)
jets = getLeadingJets[evt];

extrajets =Cases[ Select[Cases[evt,oJet],notMatch[#][jets]&],x_/;And[pT[x]>jetptmin,Abs[etaOf[x]]<jetetamax]];

(* If there's no neutrino momentum solution, return "No Solution." in place of \[Eta]. *)
If[sinh\[Eta]=="No Solution.",Return[{0,{lep,jets,ReplacePart[met,3->sinh\[Eta]]}}],met=ReplacePart[met,3->ArcSinh[sinh\[Eta]]]];


{fourLength[Plus@@(fourVector/@Join[{lep,met},jets])],{lep,jets,met},extrajets}

]


orderedEvtForm = {mtt_,{lep_,{jet1_,jet2_,jet3_,___},met_},extrajets___};


exportOrderedParticles[importDATfilename_,exportDATfilename_]:=If[Not[FileExistsQ[importDATfilename]],Print["File "<>importDATfilename<>" doesn't exist."],Export[exportDATfilename,orderParticles/@(fixImportedExpression/@Import[importDATfilename])]]


(* ::Section:: *)
(*functions of "ordered" events*)


getOrderedEvts[{{xsec_,n1evts_},{numcutevts_,numcutorderedevts_},orderedevts_}]:=orderedevts


(* ::Subsection:: *)
(*functions of a single ordered event*)


mttOf[{mtt_,{_, {_,_,_,___},_},___}]:=mtt


leptonCharge[{_,{{num_,typ_,eta_,phi_,pt_,jmass_,ntrk_,btag_,hadem_,dummy1_,dummy2_}, {_,_,_,___},_},___}]:=ntrk


leptonEta[{_,{{num_,typ_,eta_,phi_,pt_,jmass_,ntrk_,btag_,hadem_,dummy1_,dummy2_}, {_,_,_,___},_},___}]:=eta


leptonPhi[{_,{{num_,typ_,eta_,phi_,pt_,jmass_,ntrk_,btag_,hadem_,dummy1_,dummy2_}, {_,_,_,___},_},___}]:=phi


leadingJetEta[{_,{_, {{num_,typ_,eta_,phi_,pt_,jmass_,ntrk_,btag_,hadem_,dummy1_,dummy2_},_,_,___},_},___}]:=eta


secondJetEta[{_,{_, {_,{num_,typ_,eta_,phi_,pt_,jmass_,ntrk_,btag_,hadem_,dummy1_,dummy2_},_,___},_},___}]:=eta


leadingJetPhi[{_,{_, {{num_,typ_,eta_,phi_,pt_,jmass_,ntrk_,btag_,hadem_,dummy1_,dummy2_},_,_,___},_},___}]:=phi


secondJetPhi[{_,{_, {_,{num_,typ_,eta_,phi_,pt_,jmass_,ntrk_,btag_,hadem_,dummy1_,dummy2_},_,___},_},___}]:=phi


lepton[{_,{lep_, {_,_,_,___},_},___}]:=lep


leadingJet[{_,{_, {leadingjet_,_,_,___},_},___}]:=leadingjet


secondJet[{_,{_, {_,jet2_,_,___},_},___}]:=jet2


cm4vector[{_,{lep_, jets_,neutrino_},___}]:=Plus@@(fourVector/@Join[{lep,neutrino},jets])


nExtraJets[ptmin_,etamax_][{_,{lep_, jets_,neutrino_},extrajets___}]:=Length[Select[extrajets,And[pT[#]>ptmin,Abs[etaOf[#]]<etamax]&]]


(* ::Subsection:: *)
(*function of a list of ordered events*)


partitionByLepCharge[orderedEventList_]:=GatherBy[orderedEventList,leptonCharge]


(* ::Section:: *)
(*master export function*)


exportCutEvtsOrderedEvtsAndBinning[pgsfilename_,pythialogfilename_][cutevtsname_,orderedevtsname_,binningname_][mttbins_,etabins_]:=Module[{evts,weight,numcutevts, numevts},

If[Or[Not[FileExistsQ[pgsfilename]],Not[FileExistsQ[pythialogfilename]]],
Print["One of "<>pgsfilename<>" or "<>pythialogfilename<>" doesn't exist."];Return[]];

weight = getWeightAndNumEvtsFromLogFile[Import[pythialogfilename]];


evts = makeEnergyCorrectionAndCuts[DropComments[LoadEvents[pgsfilename]]];

numcutevts = Length[evts];

Export[cutevtsname,{weight,numcutevts,evts}];

evts = orderParticles/@evts;

evts = DeleteCases[evts,{0,{lep_,{jet1_,jet2_,jet3_,___},{num_,typ_,"No Solution.",phi_,pt_,jmass_,ntrk_,btag_,hadem_,_,_}},extrajets___}];

numevts = Length[evts];

Export[orderedevtsname,{weight,{numcutevts,numevts},evts}];

Export[binningname,{weight,numevts,doMttEtaBinning[mttbins,etabins][evts]}];

Remove[evts,weight,numcutevts, numevts]

]


orderedEvtFileForm ={{xsec_,n1evts_},{numcutevts_,numcutorderedevts_}, {{mtt_,{lep_,{jet1_,jet2_,jet3_,___},met_},extrajets___},___}};


exportCutEvtsOrderedEvtsNJetsAndBinning[pgsfilename_,pythialogfilename_][cutevtsname_,orderedevtsname_,jetbinningname20_,jetbinningname25_,jetbinningname30_,binningname_][mttbins_,etabins_]:=Module[{evts,weight,numcutevts, numevts},

If[Or[Not[FileExistsQ[pgsfilename]],Not[FileExistsQ[pythialogfilename]]],
Print["One of "<>pgsfilename<>" or "<>pythialogfilename<>" doesn't exist."];Return[]];

weight = getWeightAndNumEvtsFromLogFile[Import[pythialogfilename]];


evts = makeEnergyCorrectionAndCuts[DropComments[LoadEvents[pgsfilename]]];

numcutevts = Length[evts];

Export[cutevtsname,{weight,numcutevts,evts}];

evts = orderParticles/@evts;

evts = DeleteCases[evts,{0,{lep_,{jet1_,jet2_,jet3_,___},{num_,typ_,"No Solution.",phi_,pt_,jmass_,ntrk_,btag_,hadem_,_,_}},extrajets___}];

numevts = Length[evts];

Export[orderedevtsname,{weight,{numcutevts,numevts},evts}];

Export[jetbinningname20,{weight,numevts,doNJetsBinning[20,jetetamax][mttbins][evts]}];

Export[jetbinningname25,{weight,numevts,doNJetsBinning[25,jetetamax][mttbins][evts]}];

Export[jetbinningname30,{weight,numevts,doNJetsBinning[30,jetetamax][mttbins][evts]}];

Export[binningname,{weight,numevts,doMttEtaBinning[mttbins,etabins][evts]}];

Remove[evts,weight,numcutevts, numevts]

]


exportNJetsBinningFromOrderedEvts[orderedevtsfilename_][jetbinningname20_,jetbinningname25_,jetbinningname30_][mttbins_]:=Module[{evts,weight, numevts},

If[Not[FileExistsQ[orderedevtsfilename]],
Print["File "<>orderedevtsfilename<>" doesn't exist."];Return[]];

evts = fixImportedExpression/@Import[orderedevtsfilename];

weight = First[evts];

numevts = evts[[2]];

evts = Last[evts];

Export[jetbinningname20,{weight,numevts,doNJetsBinning[20,jetetamax][mttbins][evts]}];

Export[jetbinningname25,{weight,numevts,doNJetsBinning[25,jetetamax][mttbins][evts]}];

Export[jetbinningname30,{weight,numevts,doNJetsBinning[30,jetetamax][mttbins][evts]}];

Remove[evts,weight, numevts]

]
