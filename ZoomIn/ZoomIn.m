(* Wolfram Language Package *)

BeginPackage["ZoomIn`", { "MeshUtil`"}]
(* Exported symbols added here with SymbolName::usage *)  

findVertexNeighborhood::usage = "findVertexNeighborhood[mesh,seeds,maxDepth] finds 
mesh vertices that are at most maxDepth away from the seed vertices (vertices are 
specified by their indices)."

getVertexComponents::usage = "getVertexComponents[mesh,verts] decomposes verts into a list of 
connected components by their connectivity in mesh."

findNeighborRegion::usage = "findNeighborRegion[mesh,vertIdList,k] gives a list of k-ring neighboring 
regions around the given vertices. Each region is a list of cells (index into mesh)."

findBadRegions::usage = "findBadRegions[mesh, k] gives a list of k-ring neighboring 
regions around flipped cells. Each region is a list of cells (index into mesh)."


Begin["`Private`"] (* Begin Private Context *) 


bfs::usage = "bfs[nei,seeds,maxDepth] performs a bfs search starting from seeds 
and returns all nodes whose depth is no larger than maxDepth on the search tree. 
Node adjacency is specified by nei, where nei[[i]] is the list of neighbors of node i."
bfs[nei_, seeds_, maxDepth_]:= 
Module[{nv,vDepth,queue,top,
	v,vdep},
	nv = Length[nei];
	vDepth = ConstantArray[Null, nv];
	vDepth[[seeds]] = 0;
	queue = seeds;
	top = 1;
	(*bfs*)
	While[top <= Length[queue],
 		v = queue[[top]];
 		top += 1;
 		vdep = vDepth[[v]];
		If[vdep < maxDepth,
  			Scan[If[vDepth[[#]] === Null,
     				vDepth[[#]] = vdep + 1;
     				AppendTo[queue, #]] &,
     		nei[[v]]]
  		]
 	];
	(*select visited vertices*)
	Select[Range[nv], vDepth[[#]] =!= Null &]	
]

findVertexNeighborhood[mesh_, seeds_, maxDepth_]:=
Module[{nei},
	nei=MeshVertVertNeighborList[mesh];
	bfs[nei,seeds,maxDepth]	
]



getComponents[nei_,vertIdList_] :=
Module[{nv, vComponent, curComponent,
	stack, top, v, vComp, vertComponents,i},
  	(*init*)
	nv = Length[nei];
  	vComponent = ConstantArray[-1, nv];
  	vComponent[[vertIdList]] = 0;
  	curComponent = 0;
  	(*dfs*)
  	stack = vertIdList;
  	top = Length[stack];
 	While[top > 0,
   		v = stack[[top]];
   		top -= 1;
   		vComp = vComponent[[v]];
   		If[vComp == 0,
    		curComponent += 1;    
    		vComp = curComponent
    	];
  		Scan[
  			If[vComponent[[#]] == 0,(*unvisited*)
      		vComponent[[#]] = vComp;
      		If[top == Length[stack],
       			AppendTo[stack, #],
       			stack[[top + 1]] = #
       		];
      		top += 1;
      		] &
    		, nei[[v]]
    	]
	];
  	(**)
  	vertComponents = 
   		Table[Select[vertIdList, vComponent[[#]] == i &],
    		{i, Max[vComponent]}]
]

getVertexComponents[mesh_,vertIdList_]:=
Module[{nei},
	nei = MeshVertVertNeighborList[mesh];
	getComponents[nei, vertIdList]
]

findNeighborRegion[mesh_,vertIdList_,k_]:=
Module[{nei, regionVertIds, componentVerts, componentCells},
	(* get vertex components *)
	nei = MeshVertVertNeighborList[mesh];
	regionVertIds = bfs[nei,vertIdList,k];
	componentVerts = getComponents[nei,regionVertIds];
	(* get cell components *)
	componentCells = Map[
   		Module[{component = #},
     		Select[mesh[[2]], SubsetQ[component, #] &]
     	] &,
   	componentVerts]
]

findBadRegions[mesh_, k_]:=
Module[{badTriIds,badVertIds},
	badTriIds = FindFlippedCells[mesh];
	badVertIds = Union[Flatten[mesh[[2, badTriIds]]]];	
	findNeighborRegion[mesh,badVertIds,k]
]



End[] (* End Private Context *)

EndPackage[]