(* ::Package:: *)

(* ::Section:: *)
(*main routine*)


<<"lhe_lepjet_cutting_functions.m"


lheFile = ""; (* file extension must be .lhe or .lhe.gz *)
bannerFile = ""; (* file extension must be .txt *)
exportFile1 = ""; (* file extension  _uncuttopinfo.dat *)
exportFile2 = ""; (* file extension  _cuttopinfo.dat *)
exportFile3 = ""; (* file extension  _bininfo.dat *)


exportBinAndTopInfoSingleTop[lheFile,bannerFile][exportFile1,exportFile2,exportFile3]
