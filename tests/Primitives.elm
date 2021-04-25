module Primitives exposing
    ( faces
    , points
    , polylines
    , texturedFaces
    , texturedTriangles
    , triangles
    , trianglesIn
    )

import Angle
import Array
import Axis3d
import Expect
import Frame3d exposing (Frame3d)
import Length exposing (Meters)
import Obj.Decode as Decode exposing (ObjCoordinates)
import Point3d
import Polyline3d
import Test exposing (Test)
import TriangularMesh


type ZUpCoords
    = ZUpCoords


yUpToZUpFrame : Frame3d Meters ZUpCoords { defines : ObjCoordinates }
yUpToZUpFrame =
    Frame3d.rotateAround Axis3d.x (Angle.degrees 90) Frame3d.atOrigin


trianglesIn : Test
trianglesIn =
    Test.describe "trianglesIn"
        [ Test.test "transforms coordinates when decoding" <|
            \_ ->
                let
                    yUpResult : Result String (TriangularMesh.TriangularMesh (Point3d.Point3d Meters ObjCoordinates))
                    yUpResult =
                        Decode.decodeString Length.centimeters Decode.triangles objFile

                    zUpResult : Result String (TriangularMesh.TriangularMesh (Point3d.Point3d Meters ZUpCoords))
                    zUpResult =
                        Decode.decodeString Length.centimeters (Decode.trianglesIn yUpToZUpFrame) objFile
                in
                Result.map2
                    (\yUpMesh zUpMesh ->
                        let
                            maybeY =
                                TriangularMesh.vertex 0 yUpMesh
                                    |> Maybe.map (Point3d.toMeters >> .y)

                            maybeZ =
                                TriangularMesh.vertex 0 zUpMesh
                                    |> Maybe.map (Point3d.toMeters >> .z)
                        in
                        case ( maybeY, maybeZ ) of
                            ( Just y, Just z ) ->
                                Expect.within (Expect.Absolute 0.0001) y z

                            _ ->
                                Expect.fail "Missing vertices"
                    )
                    yUpResult
                    zUpResult
                    |> Result.withDefault (Expect.fail "Failed decoding")
        ]


triangles : Test
triangles =
    Test.describe "triangles"
        [ Test.test "extracts all the vertices and indices" <|
            \_ ->
                objFile
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
                (objFile ++ "\nf 500/18/6 5/19/6 1/20/6 2/11/6")
                    |> Decode.decodeString Length.centimeters Decode.triangles
                    |> Expect.equal (Err "Line 54: Index out of range")
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
                objFile
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
                (objFile ++ "\nf 500/18/6 5/19/6 1/20/6 2/11/6")
                    |> Decode.decodeString Length.centimeters Decode.faces
                    |> Expect.equal (Err "Line 54: Index out of range")
        , Test.test "errors when the normal index is out of range" <|
            \_ ->
                (objFile ++ "\nf 6/18/500 5/19/6 1/20/6 2/11/6")
                    |> Decode.decodeString Length.centimeters Decode.faces
                    |> Expect.equal (Err "Line 54: Index out of range")
        , Test.test "errors when the normal index is missing" <|
            \_ ->
                (objFile ++ "\nf 6/18 5/19 1/20 2/11")
                    |> Decode.decodeString Length.centimeters Decode.faces
                    |> Expect.equal (Err "Line 54: Vertex has no normal vector")
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
                objFile
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
                (objFile ++ "\nf 500/18/6 5/19/6 1/20/6 2/11/6")
                    |> Decode.decodeString Length.centimeters Decode.texturedTriangles
                    |> Expect.equal (Err "Line 54: Index out of range")
        , Test.test "errors when the uv index is out of range" <|
            \_ ->
                (objFile ++ "\nf 6/500/6 5/19/6 1/20/6 2/11/6")
                    |> Decode.decodeString Length.centimeters Decode.texturedTriangles
                    |> Expect.equal (Err "Line 54: Index out of range")
        , Test.test "errors when the uv index is missing" <|
            \_ ->
                (objFile ++ "\nf 6//6 5//6 1//6 2//6")
                    |> Decode.decodeString Length.centimeters Decode.texturedTriangles
                    |> Expect.equal (Err "Line 54: Vertex has no texture coordinates")
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
                objFile
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
                (objFile ++ "\nf 500/18/6 5/19/6 1/20/6 2/11/6")
                    |> Decode.decodeString Length.centimeters Decode.texturedFaces
                    |> Expect.equal (Err "Line 54: Index out of range")
        , Test.test "errors when the normal index is out of range" <|
            \_ ->
                (objFile ++ "\nf 6/18/500 5/19/6 1/20/6 2/11/6")
                    |> Decode.decodeString Length.centimeters Decode.texturedFaces
                    |> Expect.equal (Err "Line 54: Index out of range")
        , Test.test "errors when the normal index is missing" <|
            \_ ->
                (objFile ++ "\nf 6/18 5/19 1/20 2/11")
                    |> Decode.decodeString Length.centimeters Decode.texturedFaces
                    |> Expect.equal (Err "Line 54: Vertex missing normal vector and/or texture coordinates")
        , Test.test "errors when the uv index is out of range" <|
            \_ ->
                (objFile ++ "\nf 6/500/6 5/19/6 1/20/6 2/11/6")
                    |> Decode.decodeString Length.centimeters Decode.texturedFaces
                    |> Expect.equal (Err "Line 54: Index out of range")
        , Test.test "errors when the uv index is missing" <|
            \_ ->
                (objFile ++ "\nf 6//6 5//6 1//6 2//6")
                    |> Decode.decodeString Length.centimeters Decode.texturedFaces
                    |> Expect.equal (Err "Line 54: Vertex missing normal vector and/or texture coordinates")
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
                objFile
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
                (objFile ++ "\nl 500 5 1 2")
                    |> Decode.decodeString Length.centimeters Decode.polylines
                    |> Expect.equal (Err "Line 54: Index out of range")
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


points : Test
points =
    Test.describe "points"
        [ Test.test "extracts all points" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters Decode.points
                    |> Expect.equal
                        (Ok
                            [ Point3d.centimeters -1.260743 1.051649 1.526675
                            , Point3d.centimeters -1.090024 1.399034 0.711974
                            , Point3d.centimeters 1.0 1.0 -1.0
                            ]
                        )
        , Test.test "errors when the position index is out of range" <|
            \_ ->
                (objFile ++ "\np 500 5 1 2")
                    |> Decode.decodeString Length.centimeters Decode.points
                    |> Expect.equal (Err "Line 54: Index out of range")
        , Test.test "errors when no points were found" <|
            \_ ->
                ""
                    |> Decode.decodeString Length.centimeters Decode.points
                    |> Expect.equal (Err "No points found")
        , Test.test "errors when no points were found matching specific filtering criteria" <|
            \_ ->
                ""
                    |> Decode.decodeString Length.centimeters (Decode.object "Cube" (Decode.defaultGroup Decode.points))
                    |> Expect.equal (Err "No points found for group 'default', object 'Cube'")
        ]


objFile : String
objFile =
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
l 1 2 3
p 9 10
p 1"""
