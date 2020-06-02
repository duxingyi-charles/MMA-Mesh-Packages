(* Wolfram Language Package *)

BeginPackage["meshOptimizeQN`Path`"]
(* Exported symbols added here with SymbolName::usage *)  

$tmpDataDir::usage = "directory to put temporary files."

$solverExe::usage = "path to the QN solver executable file."


Begin["`Private`"] (* Begin Private Context *) 

$tmpDataDir = ExpandFileName["~/tmp_nlopt_lifted"]


(*$solverExe = "/Users/charlesdu/MEGAsync/lifted-nlopt/bin/nloptSolve_mac"
*)
$solverExe = "/Users/charlesdu/MEGAsync/lifted-nlopt/bin/findInjective_mac"



End[] (* End Private Context *)

EndPackage[]