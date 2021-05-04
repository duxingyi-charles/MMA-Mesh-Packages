(* Wolfram Language Package *)

BeginPackage["MeshUtil`"]
(* Exported symbols added here with SymbolName::usage *)  

MeshToMeshRegion::usage = "MeshToMeshRegion[mesh] converts mesh to built-in MeshRegion."
MeshRegionToMesh::usage = "MeshRegionToMesh[reg] converts built-in MeshRegion to mesh."

(* basic geometry *)
MeshBoundingBox::usage = "MeshBoundingBox[mesh] gives the bounding box of the mesh."

(* area related *)

SimplexVolume::usage = "SimplexVolume[pts] gives the volume of the simplex spanned by
points in the list pts. Triangles and tetrahedrons are supported. If pts is a [n * (n-1)]
matrix, the signed volume is returned, otherwise, the unsigned volume is returned."

MeshAreas::usage = "MeshAreas[mesh] gives a list of areas of the cells of the mesh."
NMeshAreas::usage = "NMeshAreas[mesh] gives a list of areas of the cells of 
the numerical mesh."

FindFlippedCells::usage = "FindFlippedCells[mesh] gives a list of indices of cells of the mesh 
whose volume is non-posivite."

(* angle related *)

SignedAngle::usage = "SignedAngle[u,v] gives the signed angle of the rotation from 2D vector u to 2D vector v."

getTriSignedAngles::usage = "getTriSignedAngles[{p1,p2,p3}] gives the signed angles at 3 vertices of 2D triangle p1p2p3."

getVertexWindings::usage = "getVertexWindings[mesh] gives a list of total signed angle (in radians) at each vertex."

getWindedVts::usage = "getWindedVts[mesh] gives {list of winded interior vertices, list of winded boundary vertices}.
An interior vertex is winded if the total signed angle around it is not 2pi. 
A boundary vertex is winded if the total signed angle around it is not in [0,2pi]."

(* connectivity and topology *)
MeshVertVertNeighborList::usage = "MeshVertVertNeighborList[mesh] returns a list of adjacent 
vertex Ids for each vertex in the mesh."

MeshEdgeList::usage = "meshEdgeList[mesh] returns a list of edges ({{vid1,vid2},{fid1,...}})."

extractBoundary2d::usage = "extractBoundary2d[mesh] extracts boundary edges (index-tuples) 
for given triangle mesh."

extractBoundary3d::usage = "extractBoundary3d[mesh] extracts boundary faces (index-tuples) \
for given tet mesh."

getBoundaryVts::usage = "getBoundaryVts[mesh] extracts boundary vertices of the given mesh. "

extractBoundaryChains::usage = "extractBoundaryChains[mesh] extracts boundary chains for given triangle mesh.
Each chain is a list of ordered vertex indices on one connected component of the boundary. The vertices are ordered
such that the interior of the mesh lies on the left the the chain."

getOneRingCellList::usage = "getOneRingFaceList[mesh] returns a list of incident cell Ids for each vertex in the mesh."


(* mesh consolidation *)
RemoveIsolatedVts::usage = "RemoveIsolatedVts[mesh] returns a new mesh 
where vertices not incident to any triangles are removed."

Begin["`Private`"] (* Begin Private Context *) 

MeshToMeshRegion[mesh_]:=If[Length[mesh[[2,1]]]==3,
	MeshRegion[mesh[[1]],Map[Triangle,mesh[[2]]]],
	MeshRegion[mesh[[1]],Map[Tetrahedron,mesh[[2]]]]
]

MeshRegionToMesh[reg_]:={MeshCoordinates[reg],Flatten[List @@@ MeshCells[reg,RegionDimension[reg]],1]}

MeshBoundingBox::targ = "No points in mesh."
MeshBoundingBox[mesh_]:=If[Length[mesh[[1]]]==0,
	Message[MeshBoundingBox::targ];{Null,Null},
	Transpose[CoordinateBounds[mesh[[1,Flatten[mesh[[2]]]]]]]]

SimplexVolume::targ = "Embedding dimension is too high, `1` points in `2`-dimension space."
SimplexVolume[pts_]:=Module[{geoDim,points},
	If[Length[pts]<2,Return[0.],geoDim=Length[pts[[1]]]];
	If[geoDim>Length[pts]-1,Message[SimplexVolume::targ,Length[pts],geoDim];Return[$Failed]];
	points = ArrayPad[pts,{{0,0},{0,Length[pts]-geoDim}},1.];
	Det[points]/(Length[pts]-1)!
]

MeshAreas[mesh_]:=Module[{pts,cells,cellPts,ndim},
	{pts,cells}=mesh;
	ndim=Length[cells//First]-1;
	pts=ArrayPad[pts,{{0,0},{0,1}},1];
	cellPts=Map[pts[[#]]&,cells];
	(Det/@cellPts)/(ndim!)
]


cTriArea::usage = "cTriArea[{p1,p2,p3}] returns area of a numerical triangle specified by the coordinates of its three vertices."
cTriArea=Compile[{{pts,_Real,2}},-pts[[1,2]]pts[[2,1]]+pts[[1,1]]pts[[2,2]]+pts[[1,2]]pts[[3,1]]-pts[[2,2]]pts[[3,1]]-pts[[1,1]]pts[[3,2]]+pts[[2,1]]pts[[3,2]],
	RuntimeAttributes->{Listable},CompilationTarget->"C",Parallelization->True]

cTetVolume::usage = "cTetVolume[{p1,p2,p3,p4}] returns volume of a nemerical tetrahedron specified by the coordinates of its four vertices."
cTetVolume=Compile[{{pts,_Real,2}},
-pts[[1,3]] pts[[2,2]] pts[[3,1]]+pts[[1,2]] pts[[2,3]] pts[[3,1]]+pts[[1,3]] pts[[2,1]] pts[[3,2]]
-pts[[1,1]] pts[[2,3]] pts[[3,2]]-pts[[1,2]] pts[[2,1]] pts[[3,3]]+pts[[1,1]] pts[[2,2]] pts[[3,3]]
+pts[[1,3]] pts[[2,2]] pts[[4,1]]-pts[[1,2]] pts[[2,3]] pts[[4,1]]-pts[[1,3]] pts[[3,2]] pts[[4,1]]
+pts[[2,3]] pts[[3,2]] pts[[4,1]]+pts[[1,2]] pts[[3,3]] pts[[4,1]]-pts[[2,2]] pts[[3,3]] pts[[4,1]]
-pts[[1,3]] pts[[2,1]] pts[[4,2]]+pts[[1,1]] pts[[2,3]] pts[[4,2]]+pts[[1,3]] pts[[3,1]] pts[[4,2]]
-pts[[2,3]] pts[[3,1]] pts[[4,2]]-pts[[1,1]] pts[[3,3]] pts[[4,2]]+pts[[2,1]] pts[[3,3]] pts[[4,2]]
+pts[[1,2]] pts[[2,1]] pts[[4,3]]-pts[[1,1]] pts[[2,2]] pts[[4,3]]-pts[[1,2]] pts[[3,1]] pts[[4,3]]
+pts[[2,2]] pts[[3,1]] pts[[4,3]]+pts[[1,1]] pts[[3,2]] pts[[4,3]]-pts[[2,1]] pts[[3,2]] pts[[4,3]],
RuntimeAttributes->{Listable},CompilationTarget->"C",Parallelization->True]


NMeshAreas[mesh_]:=Module[{pts,cells,cellPts,ndim,func},
	{pts,cells}=mesh;
	cellPts=pts[[#]]&/@cells;
	ndim=Length[cells//First]-1;
	If[ndim==2,func=cTriArea,func=cTetVolume];
	(func[cellPts])/(ndim!)
]

FindFlippedCells[mesh_]:=Pick[Range[Length[mesh[[2]]]],NonPositive[MeshAreas[mesh]]]

(* angle related *)
SignedAngle[u_, v_] := Module[{uvCos, uvSin},
  uvCos = u.v;
  uvSin = u[[1]] v[[2]] - u[[2]] v[[1]];
  ArcTan[uvCos, uvSin]
  ]

getTriSignedAngles[{p1_, p2_, p3_}] := 
 SignedAngle @@@ {{p2 - p1, p3 - p1}, {p3 - p2, p1 - p2}, {p1 - p3, p2 - p3}}

getVertexWindings[mesh_] := 
 Module[{vert, faces, vlist, angleList, windingList},
  {vert, faces} = mesh;
  vlist = vert[[#]] & /@ faces;
  angleList = getTriSignedAngles /@ vlist;
  windingList = ConstantArray[0, Length[vert]];
  MapThread[windingList[[#1]] += #2 &, {faces, angleList}];
  (**)
  windingList
  ]
  
getWindedVts[mesh_] := Module[{windings, bndryVert, interiorVert},
  windings = getVertexWindings[mesh];
  bndryVert = getBoundaryVts[mesh] // Round;
  interiorVert = Complement[Range[Length[mesh[[1]]]], bndryVert];
  (* {interior winded vert, boundary winded vert} *)
  {Select[interiorVert, 
  	Round[windings[[#]]/(2*Pi)]!=1&],
   Select[bndryVert, (windings[[#]] > 2 Pi || windings[[#]] < 0) &]}
  ]

(* connectivity and topology *)

MeshVertVertNeighborList[mesh_]:=Module[{nv,nvPerCell,cells=mesh[[2]],neighbors},
nv=Max[cells];
nvPerCell=Length[cells[[1]]];
neighbors=ConstantArray[{},nv];
Scan[Do[AppendTo[neighbors[[#[[i]]]],RotateLeft[#,i][[;;nvPerCell-1]]],{i,nvPerCell}]&,
cells];
Map[(#//Flatten//Union)&,neighbors]
]

meshHalfEdgeList::usage = "meshHalfEdgeList[mesh] returns a list of halfedges ({{vid1,vid2},{fid1,...}})."
meshHalfEdgeList[mesh_]:=Module[{faces=mesh[[2]],edges},
	edges=MapIndexed[Function[{eids,part},MapThread[{{#1,#2},part}&,{eids,RotateLeft[eids]}]],faces];
	edges=Flatten[edges,1];
	edges=Gather[edges,First[#1]==First[#2]&];
	Map[{#[[1,1]],Union @@ #[[All,2]]}&,edges]
	]

MeshEdgeList[mesh_]:=Module[{hedges,edges},
	hedges=meshHalfEdgeList[mesh];
	hedges=Map[If[#[[1,1]]>#[[1,2]],{Reverse[#[[1]]],#[[2]]},#]&,hedges];
	edges=Map[{#[[1,1]],Union @@ #[[All,2]]}&,Gather[hedges,(First[#1]==First[#2])&]];
	Map[If[Length[#[[2]]]==1,
	{
	SelectFirst[Transpose[{mesh[[2,#[[2,1]]]],RotateLeft[mesh[[2,#[[2,1]]]]]}],
	Function[{e},e==#[[1]] || e==Reverse[#[[1]]]]],
	#[[2]]
	},
	#
	]&,edges]
]

makeEdge[u_, v_] := {Min[u, v], Max[u, v]}

makeEdges[f_] := makeEdge @@ # & /@ If[Length[f] == 3,
    Partition[f[[{1, 2, 2, 3, 1, 3}]], 2],
    Partition[f[[{1, 2, 2, 3, 3, 1, 1, 4, 2, 4, 3, 4}]], 2]
]

makeFaces[t_] := Sort /@ Partition[t[[{1, 2, 3, 1, 2, 4, 1, 3, 4, 2, 3, 4}]], 3]
  
extractBoundary2d[mesh_] := Module[{
    edges, bndryedges
    },
   (*edges*)
   edges = Flatten[makeEdges /@ mesh[[2]], 1];
   bndryedges = #[[1]] & /@ Select[Tally[edges], #[[2]] == 1 &]
   ];

extractBoundary3d[mesh_] := Module[{
    faces, bndryfaces
    },
   (*edges*)
   faces = Flatten[makeFaces /@ mesh[[2]], 1];
   bndryfaces = #[[1]] & /@ Select[Tally[faces], #[[2]] == 1 &]
   ];

getBoundaryVts[mesh_] := 
  If[Length[mesh[[1]][[1]]] == 2, extractBoundary2d, 
      extractBoundary3d][mesh] // Flatten // Union;

extractBoundaryChains[mesh_]:= Module[{faces=mesh[[2]], edges, edgeCount, bndryEdges, bndryEdgeAssoc,
	 bndryVerts, visited, chains},
	 (* get boundary halfedges *)
	edges = Partition[#[[{1,2,2,3,3,1}]],2]& /@ faces;
	edges = Flatten[edges,1];
	edgeCount = <||>;
	Scan[
		If[KeyExistsQ[edgeCount, #],
			edgeCount[#] += 1;
			edgeCount[Reverse[#]] += 0.6,
			edgeCount[#] = 1;
			edgeCount[Reverse[#]] = 0.6
		] &
	, edges];
	bndryEdges = Select[edgeCount, # == 1 &] // Keys;
	bndryEdgeAssoc = AssociationThread[bndryEdges[[All,1]],bndryEdges[[All,2]]];
	
	(* trace boundary chains *)
	bndryVerts = Union[Flatten[bndryEdges]];
	visited[_] = False;
	chains = {};
	Scan[
		Module[{v = #, v0, chain},
			If[! visited[v],
				v0 = v;
				chain = Reap[
					Sow[v];
					visited[v] = True;
					v = bndryEdgeAssoc[v];
					While[v != v0, visited[v] = True;
						Sow[v];
						v = bndryEdgeAssoc[v]];
				][[2, 1]];
				AppendTo[chains, chain];
			]
		] &
	, bndryVerts];
	(**)
	chains
]

getOneRingCellList[mesh_] :=
    Module[ {verts, cells, oneRingCells},
        {verts, cells} = mesh;
        oneRingCells = ConstantArray[{}, Length[verts]];
        Do[
            Scan[AppendTo[oneRingCells[[#]],fid]&,cells[[fid]]]
         , {fid, Length[cells]}];
        oneRingCells
    ]

(* mesh consolidation *)

RemoveIsolatedVts[mesh_]:=Module[{nv,isNormal,curIdx,newVid,newV,newF},
nv=Length[mesh[[1]]];
isNormal=ConstantArray[False,nv];
Scan[(isNormal[[#]]=True)&,mesh[[2]]];
curIdx=0;
newVid=Map[If[isNormal[[#]],curIdx++;#->curIdx,Nothing]&,
Range[nv]]//Association;
newV=Pick[mesh[[1]],isNormal];
newF=Map[newVid[#]&,mesh[[2]],{2}];
{newV,newF}
]




End[] (* End Private Context *)

EndPackage[]