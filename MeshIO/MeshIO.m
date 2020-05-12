(* Wolfram Language Package *)

BeginPackage["MeshIO`"]
(* Exported symbols added here with SymbolName::usage *)  

ExportMesh::usage = "ExportMesh[filename,mesh] export mesh to an OBJ format file named filename."
ImportMesh::usage = "ImportMesh[filename] import mesh from an OBJ file named filename."

ExportMeshWithUV::usage = "ExportMeshWithUV[filename,mesh,uvmesh] export mesh to an OBJ format with uv stored in vt."
(*ImportMeshWithUV::usage = "ImportMeshWithUV[filename] reads an obj file and returns {mesh, uvmesh}"*)

ExportOVM::usage = "ExportOVM[filename,mesh] export tet mesh to an OVM file named filename."
ImportOVM::usage = "ImportOVM[filename] import tet mesh from an OVM file named filename."

ImportMSH::usage = "ImportMSH[filename] import tet mesh from a MSH file named filename."

ImportVTK::usage = "ImportVTK[filename] import tet mesh from a VTK file named filename."

ExportVTK::usage = "ExportVTK[filename,mesh(,header)] export tri/tet mesh to a VTK file named filename. 
					You can use header (optional) to describe the mesh."

Begin["`Private`"] (* Begin Private Context *) 

ExportMesh[filename_,mesh_]:=Module[{vertices=mesh[[1]],faces=mesh[[2]],data},
	If[Length[mesh[[1,1]]]==2, vertices=ArrayPad[vertices,{{0,0},{0,1}},0]];
	vertices=Map[Prepend[#,"v"]&,vertices];
	faces=Map[Prepend[#,"f"]&,faces];
	data=Join[vertices,faces];
	Export[filename,data,"Table","FieldSeparators"->" "]	
]

(*"f xx/yy/zz" --> "f xx"*)
removeSingleSlash[mesh_]:={mesh[[1]],Map[ToExpression[First[StringSplit[ToString[#],"/"]]]&,mesh[[2]],{2}]}

(*chech if list is a list of constant numbers*)
constantListQ[list_]:=Length[list]==0 || AllTrue[list,(NumberQ[#] && #==list[[1]])&]


(*ReduceIdenticalDimensions[mesh] returns a mesh with identical dimensions removed.*)
ReduceIdenticalDimensions[mesh_]:=Module[{verts=mesh[[1]],tris=mesh[[2]],identicals={},i},
	If[Length[verts]<2,Return[mesh]];
	Do[If[constantListQ[verts[[All,i]]],AppendTo[identicals,i]],{i,Length[verts[[1]]]}];
	{verts[[All,Complement[Range[Dimensions[verts][[2]]],identicals]]],tris}]

Options[ImportMesh]={"reduceDimension"->False}
ImportMesh[filename_,OptionsPattern[]]:=Module[{file,lines,vertices,faces,mesh},
	If[FailureQ[file=OpenRead[filename]],Return[$Failed]];
	lines=ReadList[file,Word,RecordLists->True];
	Close[file];
	vertices=Select[lines,(#[[1]]=="v")&];
	vertices=ImportString[StringRiffle[vertices[[All,2;;]]],"Table"];
	faces=Select[lines,(#[[1]]=="f")&];
	faces=ImportString[StringRiffle[faces[[All,2;;]]],"Table"];
	mesh={vertices,faces};
	mesh=removeSingleSlash[mesh];
	If[OptionValue["reduceDimension"],mesh=ReduceIdenticalDimensions[mesh]];
	mesh
]


ExportMeshWithUV[filename_,mesh_,uvmesh_]:=Module[{vertices=mesh[[1]],faces=mesh[[2]],uv=uvmesh[[1]],data},
	If[Length[mesh[[1,1]]]==2, vertices=ArrayPad[vertices,{{0,0},{0,1}},0]];
	vertices=Map[Prepend[#,"v"]&,vertices];
	uv=Map[Prepend[#,"vt"]&,uv];
	faces=Map[ToString[#]<>"/"<>ToString[#]&,faces,{2}];
	faces=Map[Prepend[#,"f"]&,faces];
	data=Join[vertices,uv,faces];
	Export[filename,data,"Table","FieldSeparators"->" "]	
]



ExportOVM[filename_,mesh_]:=Module[
{vertList,cellList,cellData,faceAssoc,fidx,pData,faceList,pdata,faceData,edgeAssoc,eidx,fData,fdata,edgeList,table},
{vertList,cellList}=mesh;
cellData=Apply[{{#1,#2,#4},{#1,#3,#2},{#2,#3,#4},{#1,#4,#3}}&,cellList,{1}];
cellData=Map[RotateLeft[#,Position[#,Min[#]][[1,1]]-1]&,cellData,{2}];
(*polyhetra \[Rule] faces*)
faceAssoc=<||>;
fidx=-1;
pData=Map[
Module[{a,b,c,val},
{a,b,c}=#;
val=Lookup[faceAssoc,Key[{a,b,c}]];
If[IntegerQ[val],val,
fidx+=2;
faceAssoc[{a,b,c}]=fidx;
faceAssoc[{a,c,b}]=fidx+1;
fidx
]
]&
,cellData,{2}];
faceList=Keys[faceAssoc];
faceList=faceList[[;;;;2]];
pdata=pData-1;
pdata=Map[Prepend[#,Length[#]]&,pdata];
(*faces \[Rule] edges*)
faceData=Apply[{{#1,#2},{#2,#3},{#3,#1}}&,faceList,{1}];
edgeAssoc=<||>;
eidx=-1;
fData=Map[
Module[{a,b,val},
{a,b}=#;
val=Lookup[edgeAssoc,Key[{a,b}]];
If[IntegerQ[val],val,
eidx+=2;
edgeAssoc[{a,b}]=eidx;
edgeAssoc[{b,a}]=eidx+1;
eidx
]
]&
,faceData,{2}];
fdata=fData-1;
fdata=Map[Prepend[#,Length[#]]&,fdata];
edgeList=Keys[edgeAssoc];
edgeList=edgeList[[;;;;2]];
edgeList-=1;
(*export*)
table=Join[
{{"OVM ASCII"}},
{{"Vertices"}},
{{Length[vertList]}},
vertList,
{{"Edges"}},
{{Length[edgeList]}},
edgeList,
{{"Faces"}},
{{Length[fdata]}},
fdata,
{{"Polyhedra"}},
{{Length[pdata]}},
pdata];
Export[filename,table,"Table"]
]

ImportOVM[filename_]:=Module[
{data,vBegin,eBegin,fBegin,pBegin,nv,ne,nf,np,
vData,eData,heData,fData,hfData,pData},
data=Import[filename,"Table"];
vBegin=Position[data,"Vertices"][[1,1]];
nv=data[[vBegin+1,1]];
eBegin=Position[data,"Edges"][[1,1]];
ne=data[[eBegin+1,1]];
fBegin=Position[data,"Faces"][[1,1]];
nf=data[[fBegin+1,1]];
pBegin=Position[data,"Polyhedra"][[1,1]];
np=data[[pBegin+1,1]];
(*vertices*)
vData=data[[vBegin+2;;vBegin+nv+1]];
(*edges*)
eData=data[[eBegin+2;;eBegin+ne+1]];
eData+=1;
(*half edges*)
heData=Riffle[eData,Reverse/@eData];
(*faces*)
fData=data[[fBegin+2;;fBegin+nf+1]];
fData=Rest/@fData;
fData+=1;
fData=Map[heData[[#]]&,fData];
fData=fData[[All,All,1]];
(*half faces*)
hfData=Riffle[fData,Reverse/@fData];
(*polyhedra*)
pData=data[[pBegin+2;;pBegin+np+1]];
pData=Rest/@pData;
pData+=1;
pData=Map[Module[{list,a,b,c,d},
list=hfData[[#]];
{a,b,c}=list[[1]];
d=Complement[list[[2]],{a,b,c}][[1]];
{a,b,d,c}
]&,
pData
];
(*mesh*)
{vData,pData}
];

ImportMSH[filename_]:=Module[{lines,i,nv,vData,nTet,tData},
lines=ReadList[filename,"Word",RecordLists->True];
i=1;
(*vertices*)
While[lines[[i,1]]!="$Nodes",i++];i++;
nv=ToExpression[lines[[i,2]]];
i++;
vData=lines[[i+1;;i+nv,2;;]];
vData=ImportString[StringRiffle[vData],"Table"];
(*tets*)
While[lines[[i,1]]!="$Elements",i++];i++;
nTet=ToExpression[lines[[i,2]]];
i++;
tData=lines[[i+1;;i+nTet,2;;]];
tData=ImportString[StringRiffle[tData],"Table"];
(*mesh*)
{vData,tData}
]

ImportVTK[filename_]:=Module[{lines,i,nv,vData,nTet,tData},
lines=ReadList[filename,"Word",RecordLists->True];
i=1;
While[lines[[i,1]]!="POINTS",i++];
nv=ToExpression[lines[[i,2]]];
vData=lines[[i+1;;i+nv]];
vData=ImportString[StringRiffle[vData],"Table"];
i=i+nv;
While[lines[[i,1]]!="CELLS",i++];
nTet=ToExpression[lines[[i,2]]];
tData=lines[[i+1;;i+nTet]];
tData=ImportString[StringRiffle[tData[[All,2;;]]],"Table"];
tData=tData+1;
tData=RotateLeft/@tData;
(*mesh*)
{vData,tData}
]

ExportVTK[filename_, mesh_, header_ : ""] :=
    Module[ {verts, cells, nv, nc, simplexSize, cellType, data},
        {verts, cells} = mesh;
        {nv, nc} = Length /@ mesh;
        If[ Length[verts[[1]]] == 2,
            verts = ArrayPad[verts, {{0, 0}, {0, 1}}, 0]
        ];
        simplexSize = Length[cells[[1]]];
        If[ simplexSize == 3,
            cellType = 5,(*tri*)
            cellType = 10(*tet*)
            ];
        cells = cells - 1;
        cells = ArrayPad[cells, {{0, 0}, {1, 0}}, simplexSize];
        data = Join[
          {
           StringSplit["# vtk DataFile Version 2.0", " "],
           StringSplit[header, " "],
           {"ASCII"},
           {"DATASET", "UNSTRUCTURED_GRID"},
           {"POINTS", nv, "double"}
           },
          verts,
          {{"CELLS", nc, nc (1 + simplexSize)}},
          cells,
          {{"CELL_TYPES", nc}},
          Table[cellType, nc]
          ];
        Export[filename, data, "Table", "FieldSeparators" -> " "]
    ]; 
   
End[] (* End Private Context *)

EndPackage[]