(* ::Package:: *)

(* Wolfram Language Package *)

BeginPackage["Render`", {"MeshUtil`"}]

(* Exported symbols added here with SymbolName::usage *)

ShowMesh::usage = "ShowMesh[mesh,options] shows planar tri mesh."

ShowMesh3D::usage = "ShowMesh3D[mesh, options] shows tet mesh and surface tri mesh."

render::usage = "render[mesh,(options)] render triangle mesh with (optional) options."

Begin["`Private`"](* Begin Private Context *)

(*Print[$ContextPath]*)

(* show mesh *)

(*visualization*)

Options[ShowMesh] = {"edgeColor" -> Black, "triangleStyle" -> Nothing, "handleVertexIds" -> {}, "showHandle" -> True, "handleColor" -> 
	Green, "handlePointSize" -> 0.02, "handleColors" -> {Green}, "showBad"
	 -> True,"badOpacity"->0.5,"badColor" -> Red, "badPointSize" -> 0.03, "showBadVertex" 
	-> False,  "highlightTriangleIds" -> {}, "highlightTriangleColor" -> Lighter[
	Blue], "highlightVertexIds" -> {}, "highlightPointSize" -> 0.025, "highlightVertexColor"
	 -> Blue, "BoundaryForm"->Black,
	 "PlotRange" -> All, "ImageSize" -> Automatic, "Axes" -> False
	}

ShowMesh[mesh_, opts : OptionsPattern[]] :=
	Module[
		{vertices = mesh[[1]], faces = mesh[[2]], badTriangleIds, badTriangles,goodTriangleIds,
			 badVertexIds, flowVectors, highlightVertColorList}
		,
		(*init*)
		If[OptionValue["showBad"],
			badTriangleIds = FindFlippedCells[mesh];
			goodTriangleIds = Complement[Range[Length[faces]],badTriangleIds];
			badVertexIds = faces[[badTriangleIds]] // Flatten // DeleteDuplicates
				
		];
		If[Length[OptionValue["highlightVertexIds"]] > 0,
			highlightVertColorList = OptionValue["highlightVertexColor"];
			If[!ListQ[highlightVertColorList],
				highlightVertColorList = ConstantArray[OptionValue["highlightVertexColor"
					], Length[OptionValue["highlightVertexIds"]]]
				,
				If[Length[highlightVertColorList] < Length[OptionValue["highlightVertexIds"
					]],
					highlightVertColorList = PadRight[highlightVertColorList, Length[
						OptionValue["highlightVertexIds"]], Blue]
				]
			]
		];
		(*graphics*)
		Graphics[
			{
				EdgeForm[OptionValue["edgeColor"]],FaceForm[]
				,
				(*bad triangles*)
				If[OptionValue["showBad"],
					{
					(*good triangles*)
					{OptionValue["triangleStyle"],GraphicsComplex[vertices,Triangle[faces[[goodTriangleIds]]]]},
					(*bad triangles*)
					{FaceForm[Opacity[OptionValue["badOpacity"],OptionValue["badColor"]]], GraphicsComplex[vertices,Triangle[faces[[badTriangleIds]]]]}
						},
						(*all triangles*)
						{OptionValue["triangleStyle"],GraphicsComplex[vertices,Triangle[faces]]}
				]
				,
				(*highlight triangles*)
				If[Length[OptionValue["highlightTriangleIds"]] > 0,
					{Opacity[0.5], OptionValue["highlightTriangleColor"], 
					GraphicsComplex[vertices,Triangle[faces[[OptionValue["highlightTriangleIds"]]]]]}
				]
				,
				(*bad vertices*)
				If[OptionValue["showBadVertex"],
					{PointSize[OptionValue["badPointSize"]], OptionValue["badColor"],
						 Point[vertices[[badVertexIds]]]}
				]
				,
				(*highlight vertices*)
				If[Length[OptionValue["highlightVertexIds"]] > 0,
					MapThread[{PointSize[OptionValue["highlightPointSize"]], #2, Point[
						#1]}&, {mesh[[1, OptionValue["highlightVertexIds"]]], highlightVertColorList
						}]
				]
				,
				(*handle vertices*)
				If[OptionValue["showHandle"] && Length[OptionValue["handleVertexIds"
					]] > 0,
					If[Length[OptionValue["handleColors"]] == Length[OptionValue["handleVertexIds"
						]],
						{PointSize[OptionValue["handlePointSize"]], MapThread[{#1, Point[
							#2]}&, {OptionValue["handleColors"], mesh[[1, OptionValue["handleVertexIds"
							]]]}]}
						,
						{PointSize[OptionValue["handlePointSize"]], OptionValue["handleColor"
							], Point[mesh[[1, OptionValue["handleVertexIds"]]]]}
					]
				],
				(*boundary edges*)
				{OptionValue["BoundaryForm"],Line[Map[mesh[[1, #]]&, extractBoundary2d[mesh]]]}
			}
			,
			FilterRules[{opts}, Options[Graphics]]
		]
	]

(* show mesh 3d *)

Options[ShowMesh3D] = {"edgeColor" -> Black, "edgeOpacity" -> 0.1, "tetOpacity"
	 -> 0.1, "badOpacity" -> 0.5, "highlightOpacity" -> 0.5, "handleVertexIds"
	 -> {}, "showHandle" -> True, "handleColor" -> Green, "handlePointSize"
	 -> 0.01, "showBad" -> False, "badColor" -> Red, "badPointSize" -> 0.005,
	 "highlightTriangleIds" -> {}, "highlightColor" -> Lighter[Blue], "PlotRange"
	 -> All, "ImageSize" -> Automatic, "Axes" -> False}

ShowMesh3D[mesh_, opts : OptionsPattern[]] :=
	Module[
		{vertices = mesh[[1]], cells = mesh[[2]], badCellIds, badVertexIds}
			
		,
		(*init*)
		If[OptionValue["showBad"],
			badCellIds = FindFlippedCells[mesh];
			badVertexIds = cells[[badCellIds]] // Flatten // DeleteDuplicates
		];
		(*graphics*)
		Graphics3D[
			{
				EdgeForm[{Opacity[OptionValue["edgeOpacity"]], OptionValue["edgeColor"
					]}]
				,
				(*triangles*)
				{Opacity[OptionValue["tetOpacity"]], Map[Simplex[vertices[[#]]]&,
					 cells]}
				,
				(*bad triangles*)
				If[OptionValue["showBad"],
					{Opacity[OptionValue["badOpacity"]], OptionValue["badColor"], Map[
						Simplex[vertices[[#]]]&, cells[[badCellIds]]]}
				]
				,
				(*highlight triangles*)
				If[Length[OptionValue["highlightTriangleIds"]] > 0,
					{Opacity[OptionValue["highlightOpacity"]], OptionValue["highlightColor"
						], Map[Triangle[vertices[[#]]]&, cells[[OptionValue["highlightTriangleIds"
						]]]]}
				]
				,
				(*bad vertices*)
				If[OptionValue["showBad"],
					{PointSize[OptionValue["badPointSize"]], OptionValue["badColor"],
						 Point[vertices[[badVertexIds]]]}
				]
				,
				(*handle vertices*)
				If[OptionValue["showHandle"] && Length[OptionValue["handleVertexIds"
					]] > 0,
					{PointSize[OptionValue["handlePointSize"]], OptionValue["handleColor"
						], Point[mesh[[1, OptionValue["handleVertexIds"]]]]}
				]
			}
			,
			Boxed -> False
			,
			FilterRules[{opts}, Options[Graphics3D]]
		]
	]

(* planar tri mesh *)

Clear[render2d];

render2d[mesh_, opts : OptionsPattern[]] :=
	ShowMesh[mesh, FilterRules[{opts}, Options[ShowMesh]], "edgeColor"
		 -> {Opacity[0.2, Black], Thickness[Min[0.002, 0.1 Sqrt[1.0 / Length[
		mesh[[2]]]]]]}, "showBad" -> True]

(* surface tri mesh *)

col = {184, 151, 128} / 255.0;

getRange[verts_] :=
	Max[Map[(Max[#] - Min[#])&, Transpose[verts]]];

Clear[render3d]

Options[render3d] = {"faceColor" -> Apply[RGBColor, col], "EdgeForm" 
	-> {}, "BoundaryForm"->GrayLevel[.3]};

render3d[mesh_, opts : OptionsPattern[]] :=
	Module[{verts, faces, range, bdedges},
		{verts, faces} = mesh;
		range = getRange[verts];
		bdedges = extractBoundary2d[mesh];
		Graphics3D[{{Specularity[GrayLevel[0.6], 100], Opacity[.8], EdgeForm[
			OptionValue["EdgeForm"]],(*Apply[RGBColor, col]*)OptionValue["faceColor"
			], Map[Polygon[verts[[#]]]&, faces]}, {Specularity[White, 100], 
			(*GrayLevel[.3]*)OptionValue["BoundaryForm"], 
			Map[Tube[verts[[#]], range * 0.008]&, bdedges]}}, Lighting -> "Neutral",
			 Boxed -> False]
	]

(* render *)

Clear[render];

render[mesh_, opts : OptionsPattern[]] :=
	Module[{embedDim, graphics},
		embedDim = Length[mesh[[1, 1]]];
		graphics =
			Switch[embedDim,
				2,
					render2d[mesh, opts]
				,
				3,
					render3d[mesh, opts]
			];
		Show[graphics, FilterRules[{opts}, PlotLabel]]
	]

End[] (* End Private Context *)

EndPackage[]
