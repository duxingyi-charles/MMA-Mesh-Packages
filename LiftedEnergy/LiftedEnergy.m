(* Wolfram Language Package *)

BeginPackage["LiftedEnergy`", { "MeshUtil`"}]
(* Exported symbols added here with SymbolName::usage *)

getLiftedFunction::usage = "getLiftedFunction[initMesh,restMesh,handles,alpha,form] 
returns func, where func is a function 
to compute lifted energy by evaluating func[freeV]."

getLiftedGrad::usage = "getLiftedGrad[initMesh,restMesh,handles,alpha,form] 
returns a function g, where g[freeV] evaluates to the flattend gradient vector."

getLiftedLaplacain::usage = 
"getLiftedLaplacain[initMesh,restMesh,handles,alpha,form] 
returns a function l, where l[freeV] evaluates to the Laplacian matrix of the lifted mesh."


getLiftedHessian::usage = "getLiftedHessian[initMesh,restMesh,handles,alpha,form] 
returns a function h, where h[freeV] evaluates to the flattend Hessian matrix."

$ProjectPolicies::usage = "a list of available polices for Hessian projection."

getLiftedProjectedHessian::usage = 
"getLiftedProjectedHessian[initMesh,restMesh,handles,alpha,form,(policy)] 
return a function h, where h[freeV] evaluates to the projected Hessian matrix. 
You can specify the projection policy. "



Begin["`Private`"] (* Begin Private Context *) 


(* Heron's triangle area formula and its derivatives *)
(*HeronTriArea[d1_, d2_, d3_] := 
  0.25 Sqrt[(d1 + d2 + d3)^2 - 2 (d1^2 + d2^2 + d3^2)]*)
  
HeronTriArea[d1_, d2_, d3_] := Module[{a,b,c},
	{c,b,a} = Sqrt[Sort[{d1,d2,d3}]];
	0.25 Sqrt[(a+(b+c))(c-(a-b))(c+(a-b))(a+(b-c))]
]

HeronTriAreaGrad[d1_, d2_, d3_] :=
 Module[{area},
  area = HeronTriArea[d1, d2, d3];
  {d2 + d3 - d1, d1 + d3 - d2, d1 + d2 - d3}/(16 area)
 ]

HeronTriAreaHessian[d1_, d2_, d3_] :=
 Module[{area, recipArea, grad, Hess, t},
  area = HeronTriArea[d1, d2, d3];
  recipArea = 1/area;
  grad = {d2 + d3 - d1, d1 + d3 - d2, d1 + d2 - d3} recipArea/16;
  Hess = -recipArea TensorProduct[grad, grad];
  t = {
     {-1, 1, 1},
     {1, -1, 1},
     {1, 1, -1}
     } recipArea/16;
  Hess = Hess + t;
  Hess
]

(* lifted triangle area and its derivatives *)
liftedTriArea::usage = 
"liftedTriArea[{v1,v2,v3},{r1,r2,r3}] computes the lifted triangle area. 
{v1,v2,v3} are vertex coordinates in the xy plane.
{r1,r2,r3} are squared edge length of the triangle in the uv plane (i.e. rest triangle)."

liftedTriArea[{v1_, v2_, v3_}, {r1_, r2_, r3_}] :=
 Module[{e1, e2, e3, d1, d2, d3},
  e1 = v2 - v3;
  e2 = v3 - v1;
  e3 = v1 - v2;
  d1 = e1.e1 + r1;
  d2 = e2.e2 + r2;
  d3 = e3.e3 + r3;
  HeronTriArea[d1, d2, d3]
]

liftedTriAreaGrad::usage = 
"liftedTriAreaGrad[{v1,v2,v3},{r1,r2,r3}] computes the gradient of the lifted 
triangle area wrt. {v1,v2,v3}."

liftedTriAreaGrad[{v1_, v2_, v3_}, {r1_, r2_, r3_}] :=
 Module[{e1, e2, e3, d1, d2, d3, gd, g1, g2, g3},
  e1 = v2 - v3;
  e2 = v3 - v1;
  e3 = v1 - v2;
  d1 = e1.e1 + r1;
  d2 = e2.e2 + r2;
  d3 = e3.e3 + r3;
  gd = HeronTriAreaGrad[d1, d2, d3];
  g1 = 2 (gd[[2]] (v1 - v3) + gd[[3]] (v1 - v2));
  g2 = 2 (gd[[1]] (v2 - v3) + gd[[3]] (v2 - v1));
  g3 = 2 (gd[[1]] (v3 - v2) + gd[[2]] (v3 - v1));
  {g1, g2, g3}
]


liftedTriAreaLaplacian[{v1_, v2_, v3_}, {r1_, r2_, r3_}] :=
 Module[{e1, e2, e3, d1, d2, d3,
   g1, g2, g3, Lap},
  e1 = v2 - v3;
  e2 = v3 - v1;
  e3 = v1 - v2;
  d1 = e1.e1 + r1;
  d2 = e2.e2 + r2;
  d3 = e3.e3 + r3;
  {g1, g2, g3} = HeronTriAreaGrad[d1, d2, d3];
  Lap = 2 {
     {g2 + g3, -g3, -g2},
     {-g3, g1 + g3, -g1},
     {-g2, -g1, g1 + g2}
     };
  Lap
]

myInner::usage = "myInner[mat1,mat2] computes the inner product of the input matrices,
replacing the lowest level multiplication by TensorProduct[]."
myInner[mat1_, mat2_] :=
 Module[{m, n, p, q, vdim, res, i, j, k},
  {m, n, vdim} = Dimensions[mat1];
  {p, q, vdim} = Dimensions[mat2];
  If[n != p, Return[$Failed]];
  
  res = ConstantArray[0.0, {m, q, vdim, vdim}];
  Do[
   res[[i, j]] += TensorProduct[mat1[[i, k]], mat2[[k, j]]]
   , {i, m}, {j, q}, {k, n}];
  
  res
]


liftedTriAreaHessian::usage = 
"liftedTriAreaHessian[{v1,v2,v3},{r1,r2,r3}] compute the Hessian of lifted triangle 
area wrt. {v1,v2,v3}."

liftedTriAreaHessian[{v1_, v2_, v3_}, {r1_, r2_, r3_}] :=
 Module[{dimension, e1, e2, e3, d1, d2, d3,
   area, dAdD, hAhD, \[Theta], dDdV, Hess1,
   g1, g2, g3, Lap, Hess2},
  dimension = Length[v1];
  
  e1 = v2 - v3;
  e2 = v3 - v1;
  e3 = v1 - v2;
  d1 = e1.e1 + r1;
  d2 = e2.e2 + r2;
  d3 = e3.e3 + r3;
  
  area = HeronTriArea[d1, d2, d3];
  dAdD = HeronTriAreaGrad[d1, d2, d3];
  hAhD = HeronTriAreaHessian[d1, d2, d3];
  
  \[Theta] = ConstantArray[0.0, dimension];
  dDdV = 2 {
     {\[Theta], v2 - v3, v3 - v2},
     {v1 - v3, \[Theta], v3 - v1},
     {v1 - v2, v2 - v1, \[Theta]}
     };
  
  Hess1 = myInner[dDdV\[Transpose], (hAhD.dDdV)];
  
  {g1, g2, g3} = dAdD;
  Lap = 2 {
     {g2 + g3, -g3, -g2},
     {-g3, g1 + g3, -g1},
     {-g2, -g1, g1 + g2}
     };
  Hess2 = KroneckerProduct[Lap, IdentityMatrix[dimension]];
  Hess2 = Partition[Hess2, {dimension, dimension}];
  
  Hess1 + Hess2
]


(* lifted energy and its derivatives *)

(* helpers *)

computeSquaredEdgeLength[mesh_] :=
 Module[{V, F, V1, V2, V3, E1, E2, E3, D1, D2, D3},
  {V, F} = mesh;
  V1 = V[[F[[All, 1]]]];
  V2 = V[[F[[All, 2]]]];
  V3 = V[[F[[All, 3]]]];
  E1 = V2 - V3;
  E2 = V3 - V1;
  E3 = V1 - V2;
  D1 = Map[#.# &, E1];
  D2 = Map[#.# &, E2];
  D3 = Map[#.# &, E3];
  Transpose[{D1, D2, D3}]
]


getRestD[restMesh_,form_]:=
Switch[form,
	"harmonic",	computeSquaredEdgeLength[restMesh],
	"tutte-uniform", ConstantArray[1.0, {Length[restMesh[[2]]], 3}]
]

getLiftedFunction[initMesh_,restMesh_,handles_,alpha_,form_] :=
Module[{restD, V, F, freeI, targetDim, func},
	restD = getRestD[restMesh,form];
 	restD *= alpha;
 
	{V, F} = initMesh;
 	freeI = Complement[Range[Length[V]], handles];
 	targetDim = {Length[freeI], Length[V[[1]]]};
 
 	func[freeV_ /; (MatrixQ[freeV] && Dimensions[freeV] == targetDim)] :=
    	Module[{Vlist, areaList},
   			V[[freeI]] = freeV;
   			Vlist = Map[V[[#]] &, F];
   			areaList = MapThread[liftedTriArea[#1, #2] &, {Vlist, restD}];
   			Total[areaList]
   		];
   	(**)
 	func
]


getLiftedGrad[initMesh_, restMesh_, handles_, alpha_, form_] :=
Module[{restD, V, F, nv, nf, freeI, targetDim, gFunc},
	restD = getRestD[restMesh,form];
 	restD *= alpha;
   
   	{V, F} = initMesh;
   	{nv, nf} = Length /@ initMesh;
   	freeI = Complement[Range[nv], handles];
   	targetDim = {Length[freeI], Length[V[[1]]]};
   	gFunc[freeV_ /; (MatrixQ[freeV] && 
        Dimensions[freeV] == targetDim)] :=
    Module[{Vlist, gList, gV, gFreeV,i},
     V[[freeI]] = freeV;
     Vlist = Map[V[[#]] &, F];
     gList = MapThread[liftedTriAreaGrad[#1, #2] &, {Vlist, restD}];
     gV = ConstantArray[0.0, Dimensions[V]];
     Do[gV[[F[[i]]]] += gList[[i]], {i, nf}];
     gFreeV = gV[[freeI]];
     Flatten[gFreeV]
    ];
   gFunc
];

getLiftedLaplacian[initMesh_, restMesh_, handles_, alpha_, form_] :=
 Module[{restD, V, F, nv, nf, vDim, freeI, targetDim, lFunc},
  	restD = getRestD[restMesh,form];
 	restD *= alpha;
  
  	{V, F} = initMesh;
  	{nv, nf} = Length /@ initMesh;
  	vDim = Length[V[[1]]];
  	freeI = Complement[Range[nv], handles];
  	targetDim = {Length[freeI], vDim};
  
  	lFunc[freeV_ /; (MatrixQ[freeV] && Dimensions[freeV] == targetDim)] :=
   		Module[{Vlist, Lap, lap, LapFree, face, i},
    		V[[freeI]] = freeV;
    		Vlist = Map[V[[#]] &, F];
    		Lap = ConstantArray[0, {nv, nv}, SparseArray];
    		Do[
     			lap = liftedTriAreaLaplacian[Vlist[[i]], restD[[i]]];
     			face = F[[i]];
     			Lap[[face, face]] += lap;
     		, {i, nf}];
    		LapFree = Lap[[freeI, freeI]]
    	];
  lFunc
]

getLiftedHessian[initMesh_, restMesh_, handles_, alpha_, form_] :=
Module[{restD, V, F, nv, nf, vDim, freeI, targetDim, hFunc},
	restD = getRestD[restMesh,form];
 	restD *= alpha;
  
  	{V, F} = initMesh;
  	{nv, nf} = Length /@ initMesh;
  	vDim = Length[V[[1]]];
  	freeI = Complement[Range[nv], handles];
  	targetDim = {Length[freeI], vDim};
  
  	hFunc[freeV_ /; (MatrixQ[freeV] && Dimensions[freeV] == targetDim)] :=
   		Module[{Vlist, Hess, hess, HessFree, face, i},
    		V[[freeI]] = freeV;
    		Vlist = Map[V[[#]] &, F];
    		Hess = ConstantArray[0, {nv, nv, vDim, vDim}, SparseArray];
    		Do[
     			hess = liftedTriAreaHessian[Vlist[[i]], restD[[i]]];
     			face = F[[i]];
     			Hess[[face, face]] += hess;
     		, {i, nf}];
    		HessFree = Hess[[freeI, freeI]];
    		HessFree = ArrayFlatten[HessFree]
    	];
  	hFunc
]

(* projected Hessian *)

$ProjectPolicies = {"Prev", "Policy1", "Policy5"};

getLiftedProjectedHessianPrev[initMesh_, restMesh_, handles_, alpha_, form_] :=
Module[{restD, V, F, nv, nf, vDim, freeI, targetDim, hFunc},
  	restD = getRestD[restMesh,form];
 	restD *= alpha;
  
  	{V, F} = initMesh;
  	{nv, nf} = Length /@ initMesh;
  	vDim = Length[V[[1]]];
  	freeI = Complement[Range[nv], handles];
  	targetDim = {Length[freeI], vDim};
  
  	hFunc[freeV_ /; (MatrixQ[freeV] && Dimensions[freeV] == targetDim)] :=
   	Module[{Vlist, Hess, hess, hessFlatten, eigenVals, eigenVecs,HessFree,face,i},
    	V[[freeI]] = freeV;
    	Vlist = Map[V[[#]] &, F];
    
    	Hess = ConstantArray[0, {nv, nv, vDim, vDim}, SparseArray];
    	Do[
     		hess = liftedTriAreaHessian[Vlist[[i]], restD[[i]]];
     		(*local projection to PSD*)
    		hessFlatten = ArrayFlatten[hess];
     		(*make sure matrix is symmetric*)
     		hessFlatten = 0.5 (hessFlatten + hessFlatten\[Transpose]);
     		{eigenVals, eigenVecs} = Eigensystem[hessFlatten];
     		eigenVals = Map[If[# < 0.0, 0.0, #] &, eigenVals];
     		hessFlatten = eigenVecs\[Transpose].DiagonalMatrix[eigenVals].eigenVecs;
     		hess = Partition[hessFlatten, {vDim, vDim}];
     		(**)
    		face = F[[i]];
     		Hess[[face, face]] += hess;
     	, {i, nf}];
     	
    	HessFree = Hess[[freeI, freeI]];
    	HessFree = ArrayFlatten[HessFree];
    
    	(*diagonal offset*)
    	HessFree + (1.0*^-8)* IdentityMatrix[Length[HessFree], SparseArray]
    ];
  	
  	hFunc
]


getLiftedProjectedHessianPolicy1[initMesh_, restMesh_, handles_, alpha_, form_] :=
Module[{restD, V, F, nv, nf, vDim, freeI, targetDim, hFunc,
	signedAreaHessian},
  	restD = getRestD[restMesh,form];
 	restD *= alpha;
  
 	{V, F} = initMesh;
  	{nv, nf} = Length /@ initMesh;
  	vDim = Length[V[[1]]];
  	freeI = Complement[Range[nv], handles];
  	targetDim = {Length[freeI], vDim};
  	
  	signedAreaHessian = {
  		{0, 0, 0, 0.5, 0, -0.5}, 
  		{0, 0, -0.5, 0, 0.5, 0}, 
  		{0, -0.5, 0, 0, 0, 0.5}, 
  		{0.5, 0, 0, 0, -0.5, 0}, 
  		{0, 0.5, 0, -0.5, 0, 0},
  		{-0.5, 0, 0.5, 0, 0, 0}};
  
  	hFunc[freeV_ /; (MatrixQ[freeV] && Dimensions[freeV] == targetDim)] :=
   	Module[{Vlist, Hess, hess, hessFlatten, eigenVals, eigenVecs,HessFree, face,i},
    	V[[freeI]] = freeV;
    	Vlist = Map[V[[#]] &, F];
    	Hess = ConstantArray[0, {nv, nv, vDim, vDim}, SparseArray];
    	Do[
     		hess = liftedTriAreaHessian[Vlist[[i]], restD[[i]]];
     		(*local projection to PSD*)
     		hessFlatten = ArrayFlatten[hess];
     		(*make sure matrix is symmetric*)
     		hessFlatten = 0.5 (hessFlatten + hessFlatten\[Transpose]);
     		(*only project non-area Hessian when triangl is not inverted*)
  
        	If[SimplexVolume[Vlist[[i]]] > 0, hessFlatten -= signedAreaHessian];
     
     		{eigenVals, eigenVecs} = Eigensystem[hessFlatten];
     
     		eigenVals = Map[If[# < 0.0, 0.0, #] &, eigenVals];
     		hessFlatten = eigenVecs\[Transpose].DiagonalMatrix[eigenVals].eigenVecs;
     
     		hess = Partition[hessFlatten, {vDim, vDim}];
     		(**)
     		face = F[[i]];
     		Hess[[face, face]] += hess;
     	, {i, nf}];
     	
    	HessFree = Hess[[freeI, freeI]];
    	HessFree = ArrayFlatten[HessFree];
    
    	(*diagonal offset*)
    
   		HessFree + (1.0*^-8)*IdentityMatrix[Length[HessFree], SparseArray]
    ];
    
  	hFunc
]


getLiftedProjectedHessianPolicy5[initMesh_, restMesh_, handles_, alpha_, form_] :=
Module[{restD, V, F, nv, nf, vDim, freeI, targetDim, hFunc,
	signedAreaHessian},
  	restD = getRestD[restMesh,form];
 	restD *= alpha;
  
 	{V, F} = initMesh;
  	{nv, nf} = Length /@ initMesh;
  	vDim = Length[V[[1]]];
  	freeI = Complement[Range[nv], handles];
  	targetDim = {Length[freeI], vDim};
  	
  	signedAreaHessian = {
  		{0, 0, 0, 0.5, 0, -0.5}, 
  		{0, 0, -0.5, 0, 0.5, 0}, 
  		{0, -0.5, 0, 0, 0, 0.5}, 
  		{0.5, 0, 0, 0, -0.5, 0}, 
  		{0, 0.5, 0, -0.5, 0, 0},
  		{-0.5, 0, 0.5, 0, 0, 0}};
  
  	hFunc[freeV_ /; (MatrixQ[freeV] && Dimensions[freeV] == targetDim)] :=
   	Module[{Vlist, Hess, hess, hessFlatten, eigenVals, eigenVecs,HessFree, face,i},
    	V[[freeI]] = freeV;
    	Vlist = Map[V[[#]] &, F];
    	Hess = ConstantArray[0, {nv, nv, vDim, vDim}, SparseArray];
    	Do[
     		hess = liftedTriAreaHessian[Vlist[[i]], restD[[i]]];
     		(*local projection to PSD*)
     		hessFlatten = ArrayFlatten[hess];
     		(*make sure matrix is symmetric*)
     		hessFlatten = 0.5 (hessFlatten + hessFlatten\[Transpose]);
     		(*only project non-area Hessian*)
        	hessFlatten -= signedAreaHessian;
     
     		{eigenVals, eigenVecs} = Eigensystem[hessFlatten];
     
     		eigenVals = Map[If[# < 0.0, 0.0, #] &, eigenVals];
     		hessFlatten = eigenVecs\[Transpose].DiagonalMatrix[eigenVals].eigenVecs;
     
     		hess = Partition[hessFlatten, {vDim, vDim}];
     		(**)
     		face = F[[i]];
     		Hess[[face, face]] += hess;
     	, {i, nf}];
     	
    	HessFree = Hess[[freeI, freeI]];
    	HessFree = ArrayFlatten[HessFree];
    
    	(*diagonal offset*)
   		HessFree + (1.0*^-8)*IdentityMatrix[Length[HessFree], SparseArray]
    ];
    
  	hFunc
]


getLiftedProjectedHessian[initMesh_,restMesh_,handles_,alpha_,form_,policy_:"Policy5"]:=
Module[{hFunc},
	Switch[policy,
		"Prev",
		hFunc = getLiftedProjectedHessianPrev[initMesh,restMesh,handles,alpha,form],
		"Policy1",
		hFunc = getLiftedProjectedHessianPolicy1[initMesh,restMesh,handles,alpha,form],
		"Policy5",
		hFunc = getLiftedProjectedHessianPolicy5[initMesh,restMesh,handles,alpha,form]
	];
	hFunc	
]




End[] (* End Private Context *)

EndPackage[]