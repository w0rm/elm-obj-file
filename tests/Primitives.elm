module Primitives exposing (faces, polylines, texturedFaces, texturedTriangles, triangles)

import Array
import Expect
import Length
import Obj.Decode as Decode
import Point3d
import Polyline3d
import Test exposing (Test)
import TriangularMesh


triangles : Test
triangles =
    Test.describe "triangles"
        [ Test.test "extracts all the vertices and indices" <|
            \_ ->
                cubeAndPolylines
                    |> Decode.decodeString Length.centimeters Decode.triangles
                    |> Result.map
                        (\triangularMesh ->
                            ( Array.length (TriangularMesh.vertices triangularMesh)
                            , List.length (TriangularMesh.faceIndices triangularMesh)
                            )
                        )
                    |> Expect.equal (Ok ( 8, 12 ))
        , Test.test "fails when the position index is out of range" <|
            \_ ->
                (cubeAndPolylines ++ "\nf 500/18/6 5/19/6 1/20/6 2/11/6")
                    |> Decode.decodeString Length.centimeters Decode.triangles
                    |> Expect.equal (Err "Line 52: Index out of range")
        , Test.test "fails when no faces were found" <|
            \_ ->
                ""
                    |> Decode.decodeString Length.centimeters Decode.triangles
                    |> Expect.equal (Err "No faces found")
        , Test.test "fails when no faces match specific filtering criteria" <|
            \_ ->
                ""
                    |> Decode.decodeString Length.centimeters (Decode.object "Cube" (Decode.defaultGroup Decode.triangles))
                    |> Expect.equal (Err "No faces found for group 'default', object 'Cube'")
        ]


faces : Test
faces =
    Test.describe "faces"
        [ Test.test "extracts all the vertices and indices" <|
            \_ ->
                cubeAndPolylines
                    |> Decode.decodeString Length.centimeters Decode.faces
                    |> Result.map
                        (\triangularMesh ->
                            ( Array.length (TriangularMesh.vertices triangularMesh)
                            , List.length (TriangularMesh.faceIndices triangularMesh)
                            )
                        )
                    |> Expect.equal (Ok ( 24, 12 ))
        , Test.test "errors when the position index is out of range" <|
            \_ ->
                (cubeAndPolylines ++ "\nf 500/18/6 5/19/6 1/20/6 2/11/6")
                    |> Decode.decodeString Length.centimeters Decode.faces
                    |> Expect.equal (Err "Line 52: Index out of range")
        , Test.test "errors when the normal index is out of range" <|
            \_ ->
                (cubeAndPolylines ++ "\nf 6/18/500 5/19/6 1/20/6 2/11/6")
                    |> Decode.decodeString Length.centimeters Decode.faces
                    |> Expect.equal (Err "Line 52: Index out of range")
        , Test.test "errors when the normal index is missing" <|
            \_ ->
                (cubeAndPolylines ++ "\nf 6/18 5/19 1/20 2/11")
                    |> Decode.decodeString Length.centimeters Decode.faces
                    |> Expect.equal (Err "Line 52: Vertex has no normal vector")
        , Test.test "errors when no faces were found" <|
            \_ ->
                ""
                    |> Decode.decodeString Length.centimeters Decode.faces
                    |> Expect.equal (Err "No faces found")
        , Test.test "errors when no faces were found matching specific filtering criteria" <|
            \_ ->
                ""
                    |> Decode.decodeString Length.centimeters (Decode.object "Cube" (Decode.defaultGroup Decode.faces))
                    |> Expect.equal (Err "No faces found for group 'default', object 'Cube'")
        ]


texturedTriangles : Test
texturedTriangles =
    Test.describe "texturedTriangles"
        [ Test.test "extracts all the vertices and indices" <|
            \_ ->
                cubeAndPolylines
                    |> Decode.decodeString Length.centimeters Decode.texturedTriangles
                    |> Result.map
                        (\triangularMesh ->
                            ( Array.length (TriangularMesh.vertices triangularMesh)
                            , List.length (TriangularMesh.faceIndices triangularMesh)
                            )
                        )
                    |> Expect.equal (Ok ( 20, 12 ))
        , Test.test "errors when the position index is out of range" <|
            \_ ->
                (cubeAndPolylines ++ "\nf 500/18/6 5/19/6 1/20/6 2/11/6")
                    |> Decode.decodeString Length.centimeters Decode.texturedTriangles
                    |> Expect.equal (Err "Line 52: Index out of range")
        , Test.test "errors when the uv index is out of range" <|
            \_ ->
                (cubeAndPolylines ++ "\nf 6/500/6 5/19/6 1/20/6 2/11/6")
                    |> Decode.decodeString Length.centimeters Decode.texturedTriangles
                    |> Expect.equal (Err "Line 52: Index out of range")
        , Test.test "errors when the uv index is missing" <|
            \_ ->
                (cubeAndPolylines ++ "\nf 6//6 5//6 1//6 2//6")
                    |> Decode.decodeString Length.centimeters Decode.texturedTriangles
                    |> Expect.equal (Err "Line 52: Vertex has no texture coordinates")
        , Test.test "errors when no faces were found" <|
            \_ ->
                ""
                    |> Decode.decodeString Length.centimeters Decode.texturedTriangles
                    |> Expect.equal (Err "No faces found")
        , Test.test "errors when no faces were found matching specific filtering criteria" <|
            \_ ->
                ""
                    |> Decode.decodeString Length.centimeters (Decode.object "Cube" (Decode.defaultGroup Decode.texturedTriangles))
                    |> Expect.equal (Err "No faces found for group 'default', object 'Cube'")
        ]


texturedFaces : Test
texturedFaces =
    Test.describe "texturedFaces"
        [ Test.test "extracts all the vertices and indices" <|
            \_ ->
                cubeAndPolylines
                    |> Decode.decodeString Length.centimeters Decode.texturedFaces
                    |> Result.map
                        (\triangularMesh ->
                            ( Array.length (TriangularMesh.vertices triangularMesh)
                            , List.length (TriangularMesh.faceIndices triangularMesh)
                            )
                        )
                    |> Expect.equal (Ok ( 24, 12 ))
        , Test.test "errors when the position index is out of range" <|
            \_ ->
                (cubeAndPolylines ++ "\nf 500/18/6 5/19/6 1/20/6 2/11/6")
                    |> Decode.decodeString Length.centimeters Decode.texturedFaces
                    |> Expect.equal (Err "Line 52: Index out of range")
        , Test.test "errors when the normal index is out of range" <|
            \_ ->
                (cubeAndPolylines ++ "\nf 6/18/500 5/19/6 1/20/6 2/11/6")
                    |> Decode.decodeString Length.centimeters Decode.texturedFaces
                    |> Expect.equal (Err "Line 52: Index out of range")
        , Test.test "errors when the normal index is missing" <|
            \_ ->
                (cubeAndPolylines ++ "\nf 6/18 5/19 1/20 2/11")
                    |> Decode.decodeString Length.centimeters Decode.texturedFaces
                    |> Expect.equal (Err "Line 52: Vertex missing normal vector and/or texture coordinates")
        , Test.test "errors when the uv index is out of range" <|
            \_ ->
                (cubeAndPolylines ++ "\nf 6/500/6 5/19/6 1/20/6 2/11/6")
                    |> Decode.decodeString Length.centimeters Decode.texturedFaces
                    |> Expect.equal (Err "Line 52: Index out of range")
        , Test.test "errors when the uv index is missing" <|
            \_ ->
                (cubeAndPolylines ++ "\nf 6//6 5//6 1//6 2//6")
                    |> Decode.decodeString Length.centimeters Decode.texturedFaces
                    |> Expect.equal (Err "Line 52: Vertex missing normal vector and/or texture coordinates")
        , Test.test "errors when no faces were found" <|
            \_ ->
                ""
                    |> Decode.decodeString Length.centimeters Decode.texturedFaces
                    |> Expect.equal (Err "No faces found")
        , Test.test "errors when no faces were found matching specific filtering criteria" <|
            \_ ->
                ""
                    |> Decode.decodeString Length.centimeters (Decode.object "Cube" (Decode.defaultGroup Decode.texturedFaces))
                    |> Expect.equal (Err "No faces found for group 'default', object 'Cube'")
        ]


polylines : Test
polylines =
    Test.describe "polylines"
        [ Test.test "extracts all points" <|
            \_ ->
                cubeAndPolylines
                    |> Decode.decodeString Length.centimeters Decode.polylines
                    |> Expect.equal
                        (Ok
                            [ Polyline3d.fromVertices
                                [ Point3d.centimeters -1.260743 1.051649 1.526675
                                , Point3d.centimeters -1.090024 1.399034 0.711974
                                ]
                            , Polyline3d.fromVertices
                                [ Point3d.centimeters 1.0 1.0 -1.0
                                , Point3d.centimeters 1.0 -1.0 -1.0
                                , Point3d.centimeters 1.0 1.0 1.0
                                ]
                            ]
                        )
        , Test.test "errors when the position index is out of range" <|
            \_ ->
                (cubeAndPolylines ++ "\nl 500 5 1 2")
                    |> Decode.decodeString Length.centimeters Decode.polylines
                    |> Expect.equal (Err "Line 52: Index out of range")
        , Test.test "errors when no lines were found" <|
            \_ ->
                ""
                    |> Decode.decodeString Length.centimeters Decode.polylines
                    |> Expect.equal (Err "No lines found")
        , Test.test "errors when no lines were found matching specific filtering criteria" <|
            \_ ->
                ""
                    |> Decode.decodeString Length.centimeters (Decode.object "Cube" (Decode.defaultGroup Decode.polylines))
                    |> Expect.equal (Err "No lines found for group 'default', object 'Cube'")
        ]


cubeAndPolylines : String
cubeAndPolylines =
    """# Blender v2.80 (sub 75) OBJ File: ''
# www.blender.org
mtllib untitled.mtl
o Cube
v 1.000000 1.000000 -1.000000
v 1.000000 -1.000000 -1.000000
v 1.000000 1.000000 1.000000
v 1.000000 -1.000000 1.000000
v -1.000000 1.000000 -1.000000
v -1.000000 -1.000000 -1.000000
v -1.000000 1.000000 1.000000
v -1.000000 -1.000000 1.000000
v -1.260743 1.051649 1.526675
v -1.090024 1.399034 0.711974
v -0.949402 2.107505 -0.479652
vt 0.375000 0.000000
vt 0.625000 0.000000
vt 0.625000 0.250000
vt 0.375000 0.250000
vt 0.375000 0.250000
vt 0.625000 0.250000
vt 0.625000 0.500000
vt 0.375000 0.500000
vt 0.625000 0.750000
vt 0.375000 0.750000
vt 0.625000 0.750000
vt 0.625000 1.000000
vt 0.375000 1.000000
vt 0.125000 0.500000
vt 0.375000 0.500000
vt 0.375000 0.750000
vt 0.125000 0.750000
vt 0.625000 0.500000
vt 0.875000 0.500000
vt 0.875000 0.750000
vn 0.0000 1.0000 0.0000
vn 0.0000 0.0000 1.0000
vn -1.0000 0.0000 0.0000
vn 0.0000 -1.0000 0.0000
vn 1.0000 0.0000 0.0000
vn 0.0000 0.0000 -1.0000
usemtl Material
s off
f 1/1/1 5/2/1 7/3/1 3/4/1
f 4/5/2 3/6/2 7/7/2 8/8/2
f 8/8/3 7/7/3 5/9/3 6/10/3
f 6/10/4 2/11/4 4/12/4 8/13/4
f 2/14/5 1/15/5 3/16/5 4/17/5
f 6/18/6 5/19/6 1/20/6 2/11/6
l 9 10
l 1 2 3"""
