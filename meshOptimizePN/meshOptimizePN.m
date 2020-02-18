(* Wolfram Language Package *)

BeginPackage["meshOptimizePN`" , {"MeshUtil`"}]
(* Exported symbols added here with SymbolName::usage *)  

$tmpDataDir::usage = "directory to put temporary files."

$solverExe::usage = "path to the PN solver executable file."

$recordOptions::usage = "list of available record options."

computeAlpha::usage = "computeAlpha[mesh,restMesh,alphaRatio,form] computes the value of 
alpha given the input meshes, alphaRatio and energy form."

liftedFormulation::usage = "liftedFormulation[(opts)] specifies options for the lifted energy.
The options include alphaRatio, alpha and form. The option form can take value from harmonic 
and tutte-uniform."

mySolver::usage = "mySolver[(opts)] specifies options for PN solver.
Please use Options[mySolver] to see the available options and their default values."

meshOptimize::usage = "meshOptimize[mesh,restMesh,handles,formulation,(opts)] optimizes the 
formulation energy on the input data using the solver specified in opts. The default solver is
mySolver."

printInfo::usage = "printInfo[result] prints basic information of optimization result."

Begin["`Private`"] (* Begin Private Context *) 

(*Print[$ContextPath]*)

(* Formulation related *)

$tmpDataDir = ExpandFileName["~/tmp_Eigen_lifted"]

$solverExe = 
  "/Users/charlesdu/Downloads/research/lifted-eigen/lifted_5"
  
$recordOptions = {"vert", "energy", "minArea", "gradient", "searchDirection"}

Clear[exportFormulationData]
exportFormulationData[filename_, restM_, initM_, hdls_, form_, 
  alpha_] :=
 Module[{restV, F, initV, data},
  restV = N[restM[[1]]];
  F = Round[restM[[2]]];
  initV = N[initM[[1]]];
  data = Join[
    {Dimensions[restV]},
    restV,
    {Dimensions[initV]},
    initV,
    {Dimensions[F]},
    F - 1,
    {Length[hdls]},
    hdls - 1,
    {form, N[alpha]}
    ];
  Export[filename, data, "Table"]
 ]


computeAlpha[mesh_,restMesh_,alphaRatio_,form_]:=
Module[{simplexSize,restMeasure,\[Alpha]},
   simplexSize = Last[Dimensions[mesh[[2]]]];(*3:tri mesh. 4:tet mesh*)
   Switch[form,
     "harmonic",
     If[simplexSize == 3,
      restMeasure = Area[MeshToMeshRegion[restMesh]],
      restMeasure = Volume[MeshToMeshRegion[restMesh]]],
     "tutte-uniform",
     If[simplexSize == 3,
      restMeasure = Length[restMesh[[2]]] \[Sqrt]3/4.0,
      restMeasure = Length[restMesh[[2]]] \[Sqrt]2/12.0]
    ];

    \[Alpha] = alphaRatio * Total[NMeshAreas[mesh]]/restMeasure
]

Clear[liftedFormulation]
Options[liftedFormulation] = {"alphaRatio" -> 1.0, 
   "alpha" -> Automatic, "form" -> "harmonic"};
liftedFormulation[opts : OptionsPattern[]] :=
 Function[{mesh, restMesh, handles},
  Module[{\[Alpha], filename},
   (*compute alpha*)
   If[OptionValue["alpha"] === Automatic,
    	\[Alpha] = computeAlpha[mesh,restMesh,OptionValue["alphaRatio"],OptionValue["form"]];
    	Echo[OptionValue["alphaRatio"], "alphaRatio "],
    	(*specified alpha*)
    	\[Alpha] = OptionValue["alpha"]
   ];
   Echo[Evaluate[\[Alpha]], "alpha "];
   (* write data file *)
   
   filename = FileNameJoin[{$tmpDataDir, "lifted_" <> DateString[]}];
   While[FileExistsQ[filename], 
    filename = 
     FileNameJoin[{$tmpDataDir, "lifted_" <> DateString[]}]];
   exportFormulationData[filename, restMesh, mesh, handles, 
    OptionValue["form"], \[Alpha]];
   (**)
   filename
   ]
 ]
 
(* Solver related *) 


Clear[exportSolverOptions]
Options[exportSolverOptions] = {
   "AccuracyGoal" -> Automatic, "PrecisionGoal" -> Automatic,
   "GradientGoal" -> Automatic,
   "Method" -> "Projected_Newton", "MaxIterations" -> 1000,
   "stopCode" -> "none", "record" -> {}
   };
exportSolverOptions[filename_, opts : OptionsPattern[]] :=
 Module[{a, p, ftolAbs, ftolRel, xtolAbs, xtolRel, gtol},
  (**)
  a = OptionValue["AccuracyGoal"];
  If[a === Automatic,
   a = 1.0*^-8,
   If[a === Infinity,
    a = -1,
    a = Power[10.0, -a]
    ]
   ];
  ftolAbs = a;
  xtolAbs = a;
  (**)
  p = OptionValue["PrecisionGoal"];
  If[p === Automatic,
   p = 1.0*^-8,
   If[p === Infinity,
    p = -1,
    p = Power[10.0, -p]
    ]
   ];
  ftolRel = p;
  xtolRel = p;
  (**)
  gtol = OptionValue["GradientGoal"];
  If[gtol === Automatic,
   gtol = 1.0*^-8,
   If[gtol === Infinity,
    gtol = -1,
    gtol = Power[10.0, -gtol]
    ]
   ];
  (* write options *)
  Export[filename,
   Join[{"ftol_abs", ftolAbs,
     "ftol_rel", ftolRel,
     "xtol_abs", xtolAbs,
     "xtol_rel", xtolRel,
     "gtol_abs", gtol,
     "algorithm", OptionValue["Method"],
     "maxeval", OptionValue["MaxIterations"],
     "stopCode", OptionValue["stopCode"],
     "record"},
    Prepend[OptionValue["record"], Length[OptionValue["record"]]]
    ],
   "List"];
]


Clear[importExperimentResult]
importExperimentResult[filename_] :=
 Module[{titleList, stream, lines, res, i, line, title, dims, data},
  titleList = {"resV", "vert", "energy", "minArea", "gradient", 
    "searchDirection"};
  If[FailureQ[stream = OpenRead[filename]], Return[$Failed]];
  lines = ReadList[stream, Word, RecordLists -> True];
  Close[stream];
  res = <||>;
  i = 1;
  While[i <= Length[lines],
   line = lines[[i]]; i += 1;
   If[Length[line] > 1 && MemberQ[titleList, line[[1]]],
    title = line // First;
    dims = line // Rest // ToExpression;
    line = lines[[i]]; i += 1;
    data = ImportString[StringRiffle[line], "Table"];
    data = ArrayReshape[data, dims];
    AssociateTo[res, title -> data]
    ]
   ];
  res
]

Clear[mySolver]
Options[mySolver] = Options[exportSolverOptions];
mySolver[opts : OptionsPattern[]] :=
 Function[{dataFileName},
  Module[{optFileName, resFileName, time, info, res},
   (*export solver options*)
   
   optFileName = dataFileName <> "_solver_options";
   resFileName = dataFileName <> "_res";
   exportSolverOptions[optFileName, 
    Evaluate[FilterRules[{opts}, Options[mySolver]]]];
   (*optimize*)
   {time, info} = 
    RunProcess[{$solverExe, dataFileName, optFileName, resFileName}] //
      AbsoluteTiming;
   If[info["ExitCode"] =!= 0, Print[info]; Return[$Failed]];
   (*import result*)
   res = importExperimentResult[resFileName];
   (*delete tmp files*)
   
   DeleteFile[{dataFileName, optFileName, resFileName}];
   (*return*)
   If[res === $Failed,
    $Failed,
    AssociateTo[
     res, {"timing" -> Quantity[time, "Seconds"], "info" -> info}]
    ]
   ]
]

(* Mesh Optimize *)

Clear[meshOptimize]
Options[meshOptimize] = {"solver" -> Automatic};
meshOptimize[mesh_, restMesh_, handles_, formulation_, 
  opts : OptionsPattern[]] :=
 
 Module[{dataFileName, solver, resMesh, 
   result},
  (*write problem formulation data*)
  
  dataFileName = formulation[mesh, restMesh, handles];
  (**)
  If[OptionValue["solver"] === Automatic,
   solver = mySolver[],
   solver = OptionValue["solver"]];
  (*solve*)
  (*result = <|resV\[Rule]..., energy\[Rule]..., ...|>*)
 
   result = solver[dataFileName];
  If[result === $Failed,
   $Failed,
   resMesh = {result["resV"], mesh[[2]]};
   AssociateTo[result, "resMesh" -> resMesh]
   ]
]

printInfo[result_]:=
Print[
	If[KeyExistsQ[result,"energy"],
		ToString[Length[result["energy"]]]<>" iters, "<>
		"fval="<>ToString[CForm[Last[result["energy"]]]]<>", ",
		""]
	<>
	If[KeyExistsQ[result,"timing"],
		ToString[result["timing"]]<>", ",
		""
	]
	<>
	If[KeyExistsQ[result,"resMesh"],
		ToString[Length[FindFlippedCells[result["resMesh"]]]]<>" flipped."
	]
]


End[] (* End Private Context *)

EndPackage[]