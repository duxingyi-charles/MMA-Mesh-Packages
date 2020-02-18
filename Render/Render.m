(* Wolfram Language Package *)

BeginPackage["Render`", {"MeshUtil`"}]
(* Exported symbols added here with SymbolName::usage *)  

ShowMesh::usage = "ShowMesh[mesh,options] shows planar tri mesh."
ShowMesh3D::usage = "ShowMesh3D[mesh, options] shows tet mesh and surface tri mesh."

render::usage = "render[mesh,(title)] render triangle mesh with (optional) title."


Begin["`Private`"] (* Begin Private Context *) 

(*Print[$ContextPath]*)

(* show mesh *)
(*visualization*)
Options[ShowMesh]={"edgeColor"->Black,
	"handleVertexIds"->{},"showHandle"-> True,"handleColor"->Green,"handlePointSize"->0.02,
	"showBad"->True,"badColor"->Red,"badPointSize"->0.03,"showBadVertex"->False,
	"showFlowVector"->False,"flowVectorColor"->Blue,"flowVectorScale"->0.5,
	"highlightTriangleIds"->{},"highlightTriangleColor"->Lighter[Blue],
	"highlightVertexIds"->{},"highlightPointSize"->0.025,"highlightVertexColor"->Blue,
	"PlotRange"->All,"ImageSize"->Automatic}

ShowMesh[mesh_,opts:OptionsPattern[]]:=Module[
	{vertices=mesh[[1]],faces=mesh[[2]],badTriangleIds,badTriangles,badVertexIds,flowVectors,highlightVertColorList},
	(*init*)
	If[OptionValue["showBad"]\[Or]OptionValue["showFlowVector"],
		badTriangleIds=FindFlippedCells[mesh];
		badVertexIds=faces[[badTriangleIds]]//Flatten//DeleteDuplicates];
	If[OptionValue["showFlowVector"],
		flowVectors=ConstantArray[{0.,0.},Length[vertices]];
		badTriangles=faces[[badTriangleIds]];
		Scan[Do[flowVectors[[#[[i]]]]+=Cross[vertices[[#[[Mod[i+2,3,1]]]]]-vertices[[#[[Mod[i+1,3,1]]]]]],{i,3}]&,badTriangles]
	];
	If[Length[OptionValue["highlightVertexIds"]]>0,
		highlightVertColorList=OptionValue["highlightVertexColor"];
		If[!ListQ[highlightVertColorList],
			highlightVertColorList=ConstantArray[OptionValue["highlightVertexColor"],Length[OptionValue["highlightVertexIds"]]],
			If[Length[highlightVertColorList]<Length[OptionValue["highlightVertexIds"]],
				highlightVertColorList=PadRight[highlightVertColorList,Length[OptionValue["highlightVertexIds"]],Blue]
			]
		]
	];
	(*graphics*)
	Graphics[{EdgeForm[OptionValue["edgeColor"]],
		(*triangles*)
		{Opacity[0.],Map[Triangle[vertices[[#]]]&,faces[[Range[Length[faces]]]]]},
		(*bad triangles*)
		If[OptionValue["showBad"],{Opacity[0.5],OptionValue["badColor"],EdgeForm[Opacity[1.0,OptionValue["badColor"]]],Map[Triangle[vertices[[#]]]&,faces[[badTriangleIds]]]}], 
		(*highlight triangles*)
		If[Length[OptionValue["highlightTriangleIds"]]>0,
			{Opacity[0.5],OptionValue["highlightTriangleColor"],Map[Triangle[vertices[[#]]]&,faces[[OptionValue["highlightTriangleIds"]]]]}],
		(*bad vertices*)
		If[OptionValue["showBadVertex"],{PointSize[OptionValue["badPointSize"]],OptionValue["badColor"],Point[vertices[[badVertexIds]]]}],
		(*handle vertices*)
		If[OptionValue["showHandle"]&&Length[OptionValue["handleVertexIds"]]>0,
			{PointSize[OptionValue["handlePointSize"]],OptionValue["handleColor"],Point[mesh[[1,OptionValue["handleVertexIds"]]]]}],
		(*highlight vertices*)
		If[Length[OptionValue["highlightVertexIds"]]>0,
			MapThread[{PointSize[OptionValue["highlightPointSize"]],#2,Point[#1]}&,
			{mesh[[1,OptionValue["highlightVertexIds"]]],highlightVertColorList}]
		],
		(*flow vectors*)
		If[OptionValue["showFlowVector"],
			{OptionValue["flowVectorColor"],Map[Arrow[{vertices[[#]],vertices[[#]]+OptionValue["flowVectorScale"]*flowVectors[[#]]}]&,badVertexIds]}]
	},
	FilterRules[{opts},Options[Graphics]]]
]

(* show mesh 3d *)
Options[ShowMesh3D]={"edgeColor"->Black,"edgeOpacity"->0.1,"tetOpacity"->0.1,"badOpacity"->0.5,"highlightOpacity"->0.5,
	"handleVertexIds"->{},"showHandle"-> True,"handleColor"->Green,"handlePointSize"->0.01,
	"showBad"->False,"badColor"->Red,"badPointSize"->0.005,
	"highlightTriangleIds"->{},"highlightColor"->Lighter[Blue],
	"PlotRange"->All,"ImageSize"->Automatic}
	
ShowMesh3D[mesh_,opts:OptionsPattern[]]:=Module[
	{vertices=mesh[[1]],cells=mesh[[2]],badCellIds,badVertexIds},
	(*init*)
	If[OptionValue["showBad"],
		badCellIds=FindBadTriangles[mesh];
		badVertexIds=cells[[badCellIds]]//Flatten//DeleteDuplicates];	
	(*graphics*)
	Graphics3D[{EdgeForm[{Opacity[OptionValue["edgeOpacity"]],OptionValue["edgeColor"]}],
		(*triangles*)
		{Opacity[OptionValue["tetOpacity"]],Map[Simplex[vertices[[#]]]&,cells]},
		(*bad triangles*)
		If[OptionValue["showBad"],{Opacity[OptionValue["badOpacity"]],OptionValue["badColor"],Map[Simplex[vertices[[#]]]&,cells[[badCellIds]]]}], 
		(*highlight triangles*)
		If[Length[OptionValue["highlightTriangleIds"]]>0,
			{Opacity[OptionValue["highlightOpacity"]],OptionValue["highlightColor"],Map[Triangle[vertices[[#]]]&,cells[[OptionValue["highlightTriangleIds"]]]]}],
		(*bad vertices*)
		If[OptionValue["showBad"],{PointSize[OptionValue["badPointSize"]],OptionValue["badColor"],Point[vertices[[badVertexIds]]]}],
		(*handle vertices*)
		If[OptionValue["showHandle"]&&Length[OptionValue["handleVertexIds"]]>0,
			{PointSize[OptionValue["handlePointSize"]],OptionValue["handleColor"],Point[mesh[[1,OptionValue["handleVertexIds"]]]]}]
	},Boxed->False,
	FilterRules[{opts},Options[Graphics3D]]]
]

(* planar tri mesh *)

render2d[mesh_] :=
  Show[
   ShowMesh[mesh
    , "edgeColor" -> {
      Opacity[0.3, Black]
      , Thickness[Min[0.002, 0.1 Sqrt[1.0/Length[mesh[[2]]]]]]
      }
    (*, "showBadVertex" -> True, "badPointSize" -> 0.01*)
    , "showBad" -> True
    ]
   ,
   Graphics[Line[Map[mesh[[1, #]] &, extractBoundary2d[mesh]]]]
]


(* surface tri mesh *)

col = {184, 151, 128}/255.0;

getRange[verts_] := Max[Map[(Max[#] - Min[#]) &, Transpose[verts]]];

render3d[mesh_] := 
 Module[{verts, faces, range, bdedges},
 	{verts, faces} = mesh;
 	range = getRange[verts];
 	bdedges = extractBoundary2d[mesh];
  Graphics3D[{{Specularity[GrayLevel[0.6], 100], Opacity[.8], 
     EdgeForm[{Black}], Apply[RGBColor, col], 
     Map[Polygon[verts[[#]]] &, faces]},
    {Specularity[White, 100], GrayLevel[.3], 
     Map[Tube[verts[[#]], range*0.008] &, bdedges]}},
   Lighting -> "Neutral", Boxed -> False]]

(* render *)

render[mesh_,title_:""] := 
Module[{embedDim,graphics},
	embedDim=Length[mesh[[1,1]]];
	graphics=Switch[embedDim,
		2, render2d[mesh],
		3, render3d[mesh]];
	Show[graphics,PlotLabel->title]
]


End[] (* End Private Context *)

EndPackage[]