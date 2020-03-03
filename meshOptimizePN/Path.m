(* Wolfram Language Package *)

BeginPackage["meshOptimizePN`Path`"]
(* Exported symbols added here with SymbolName::usage *)  

$tmpDataDir::usage = "directory to put temporary files."

$solverExe::usage = "path to the PN solver executable file."


Begin["`Private`"] (* Begin Private Context *) 

$tmpDataDir = ExpandFileName["~/tmp_Eigen_lifted"]

$solverExe = "/Users/charlesdu/Downloads/research/lifted-eigen/cmake-build-release-clang/lifted_exe_PN1_stepParam0.5"


End[] (* End Private Context *)

EndPackage[]