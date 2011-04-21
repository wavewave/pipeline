(* ::Package:: *)

(* ::Section:: *)
(*main routine*)


<<"lhe_lepjet_cutting_functions.m"


lheFile = "$lheFile$"; (* file extension must be .lhe or .lhe.gz *)
bannerFile = "$bannerFile$"; (* file extension must be .txt *)
exportFile1 = "$exportFileOne$"; (* file extension  _uncuttopinfo.dat *)
exportFile2 = "$exportFileTwo$"; (* file extension  _cuttopinfo.dat *)
exportFile3 = "$exportFileThree$"; (* file extension  _bininfo.dat *)


exportBinAndTopInfo[lheFile,bannerFile][exportFile1,exportFile2,exportFile3]
