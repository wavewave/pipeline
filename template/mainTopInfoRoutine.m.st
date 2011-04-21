(* ::Package:: *)

(* ::Section:: *)
(*main routine*)


<<"$TopInfoExportPkgFile$" (* "TopInfoExport.m" *)


lheFile = "$lheFile$"; (* file extension must be .lhe or .lhe.gz *)
bannerFile = "$bannerFile$"; (* file extension must be .txt *)
exportFile1 = "$exportFileTopInfo$"; (* file extension should be ..._topinfo.dat *)
exportFile2 = "$exportFileSuccintInfo$"; (* file extension must be ..._succinctinfo.dat *)


exportTopInfo3[lheFile,bannerFile][exportFile1,exportFile2]
