module Encoding exposing
    ( compact
    , faces
    , multipart
    , options
    , points
    , polylines
    , texturedFaces
    , texturedTriangles
    , triangles
    )

import Array
import Expect
import Length exposing (Meters)
import Obj.Encode as Encode exposing (Options, defaultOptions)
import Point3d exposing (Point3d)
import Polyline3d
import Quantity exposing (Unitless)
import Test exposing (Test)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


triangles : Test
triangles =
    Test.describe "triangles"
        [ Test.test "correctly writes positions and indices" <|
            \_ ->
                Encode.encode Length.inMeters (Encode.triangles trianglesSquare)
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "f 1 2 3\n"
                            , "f 1 3 4\n"
                            ]
                        )
        , Test.test "skips empty vertices" <|
            \_ ->
                Encode.encode Length.inMeters
                    (Encode.triangles
                        (TriangularMesh.indexed Array.empty [ ( 1, 2, 3 ) ])
                    )
                    |> Expect.equal ""
        , Test.test "skips empty indices" <|
            \_ ->
                Encode.encode Length.inMeters
                    (Encode.triangles
                        (TriangularMesh.indexed (Array.fromList [ Point3d.meters 1 2 3 ]) [])
                    )
                    |> Expect.equal ""
        ]


faces : Test
faces =
    Test.describe "faces"
        [ Test.test "correctly writes positions, normal vectors and indices" <|
            \_ ->
                Encode.encode Length.inMeters (Encode.faces texturedFacesSquare)
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "f 1//1 2//2 3//3\n"
                            , "f 1//1 3//3 4//4\n"
                            ]
                        )
        , Test.test "skips empty vertices" <|
            \_ ->
                Encode.encode Length.inMeters
                    (Encode.faces (TriangularMesh.indexed Array.empty [ ( 1, 2, 3 ) ]))
                    |> Expect.equal ""
        , Test.test "skips empty indices" <|
            \_ ->
                Encode.encode Length.inMeters
                    (Encode.faces
                        (TriangularMesh.indexed
                            (Array.fromList
                                [ { position = Point3d.meters -4.5 4.5 0
                                  , normal = Vector3d.unitless 0 0 1
                                  }
                                ]
                            )
                            []
                        )
                    )
                    |> Expect.equal ""
        ]


texturedTriangles : Test
texturedTriangles =
    Test.describe "texturedTriangles"
        [ Test.test "correctly writes positions, UV and indices" <|
            \_ ->
                Encode.encode Length.inMeters (Encode.texturedTriangles texturedFacesSquare)
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "vt 0.000000 1.000000\n"
                            , "vt 1.000000 1.000000\n"
                            , "vt 1.000000 0.000000\n"
                            , "vt 0.000000 0.000000\n"
                            , "f 1/1 2/2 3/3\n"
                            , "f 1/1 3/3 4/4\n"
                            ]
                        )
        , Test.test "skips empty vertices" <|
            \_ ->
                Encode.encode Length.inMeters
                    (Encode.texturedTriangles (TriangularMesh.indexed Array.empty [ ( 1, 2, 3 ) ]))
                    |> Expect.equal ""
        , Test.test "skips empty indices" <|
            \_ ->
                Encode.encode Length.inMeters
                    (Encode.texturedTriangles
                        (TriangularMesh.indexed
                            (Array.fromList
                                [ { position = Point3d.meters -4.5 4.5 0
                                  , uv = ( 0, 1 )
                                  }
                                ]
                            )
                            []
                        )
                    )
                    |> Expect.equal ""
        ]


texturedFaces : Test
texturedFaces =
    Test.describe "texturedFaces"
        [ Test.test "correctly writes positions, UV and indices" <|
            \_ ->
                Encode.encode Length.inCentimeters (Encode.texturedFaces texturedFacesSquare)
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -450.000000 450.000000 0.000000\n"
                            , "v 450.000000 450.000000 0.000000\n"
                            , "v 450.000000 -450.000000 0.000000\n"
                            , "v -450.000000 -450.000000 0.000000\n"
                            , "vt 0.000000 1.000000\n"
                            , "vt 1.000000 1.000000\n"
                            , "vt 1.000000 0.000000\n"
                            , "vt 0.000000 0.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "f 1/1/1 2/2/2 3/3/3\n"
                            , "f 1/1/1 3/3/3 4/4/4\n"
                            ]
                        )
        , Test.test "skips empty vertices" <|
            \_ ->
                Encode.encode Length.inMeters
                    (Encode.texturedFaces (TriangularMesh.indexed Array.empty [ ( 1, 2, 3 ) ]))
                    |> Expect.equal ""
        , Test.test "skips empty indices" <|
            \_ ->
                Encode.encode Length.inMeters
                    (Encode.texturedFaces
                        (TriangularMesh.indexed
                            (Array.fromList
                                [ { position = Point3d.meters -4.5 4.5 0
                                  , uv = ( 0, 1 )
                                  , normal = Vector3d.unitless 0 0 1
                                  }
                                ]
                            )
                            []
                        )
                    )
                    |> Expect.equal ""
        ]


points : Test
points =
    Test.describe "points"
        [ Test.test "correctly writes positions and indices" <|
            \_ ->
                Encode.encode Length.inMeters
                    (Encode.points
                        [ Point3d.meters -4.5 4.5 0
                        , Point3d.meters 4.5 4.5 0
                        , Point3d.meters 4.5 -4.5 0
                        , Point3d.meters -4.5 -4.5 0
                        ]
                    )
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "p 1\n"
                            , "p 2\n"
                            , "p 3\n"
                            , "p 4\n"
                            ]
                        )
        , Test.test "skips empty points" <|
            \_ ->
                Encode.encode Length.inMeters
                    (Encode.points [])
                    |> Expect.equal ""
        ]


polylines : Test
polylines =
    Test.describe "polylines"
        [ Test.test "correctly writes positions and indices" <|
            \_ ->
                Encode.encode Length.inMeters
                    (Encode.polylines
                        [ Polyline3d.fromVertices
                            [ Point3d.meters -4.5 4.5 0
                            , Point3d.meters 4.5 4.5 0
                            ]
                        , Polyline3d.fromVertices
                            [ Point3d.meters 4.5 -4.5 0
                            , Point3d.meters -4.5 -4.5 0
                            ]
                        ]
                    )
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "l 1 2\n"
                            , "l 3 4\n"
                            ]
                        )
        , Test.test "skips empty polylines" <|
            \_ ->
                Encode.encode Length.inMeters
                    (Encode.polylines [])
                    |> Expect.equal ""
        , Test.test "skips polylines without vertices" <|
            \_ ->
                Encode.encode Length.inMeters
                    (Encode.polylines [ Polyline3d.fromVertices [] ])
                    |> Expect.equal ""
        ]


options : Test
options =
    Test.describe "options"
        [ Test.test "respects the precision option" <|
            \_ ->
                Encode.encode Length.inMeters (Encode.trianglesWith { defaultOptions | precision = 3 } precisionTriangle)
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -6.000 6.000 0.123\n"
                            , "v -0.123 -1.123 0.000\n"
                            , "v 0.000 0.000 0.000\n"
                            , "f 3 2 1\n"
                            ]
                        )
        , Test.test "clamps the precision to 1" <|
            \_ ->
                Encode.encode Length.inMeters (Encode.trianglesWith { defaultOptions | precision = 0 } precisionTriangle)
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -6.0 6.0 0.1\n"
                            , "v -0.1 -1.1 0.0\n"
                            , "v 0.0 0.0 0.0\n"
                            , "f 3 2 1\n"
                            ]
                        )
        , Test.test "clamps the precision to 10" <|
            \_ ->
                Encode.encode Length.inMeters (Encode.trianglesWith { defaultOptions | precision = 20 } precisionTriangle)
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -5.9999999990 5.9999999990 0.1234567890\n"
                            , "v -0.1234567890 -1.1234567890 0.0000000000\n"
                            , "v 0.0000000000 0.0000000000 0.0000000001\n"
                            , "f 3 2 1\n"
                            ]
                        )
        , Test.test "sanitizes metadata" <|
            \_ ->
                Encode.encode Length.inMeters (Encode.pointsWith unsafeOptions [ Point3d.meters 1 2 3 ])
                    |> Expect.equal
                        (String.concat
                            [ "o ObjectName\n"
                            , "g Group1 Group2\n"
                            , "usemtl TestMaterial\n"
                            , "v 1.000000 2.000000 3.000000\n"
                            , "p 1\n"
                            ]
                        )
        ]


multipart : Test
multipart =
    Test.describe "multipart"
        [ Test.test "correctly offsets indices for triangular meshes" <|
            \_ ->
                Encode.encodeMultipart Length.inMeters
                    [ Encode.triangles trianglesSquare -- offsets positions
                    , Encode.texturedTriangles texturedFacesSquare -- offsets positions and uvs
                    , Encode.faces texturedFacesSquare -- offsets positions and normals
                    , Encode.texturedFaces texturedFacesSquare
                    ]
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "f 1 2 3\n"
                            , "f 1 3 4\n"
                            , "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "vt 0.000000 1.000000\n"
                            , "vt 1.000000 1.000000\n"
                            , "vt 1.000000 0.000000\n"
                            , "vt 0.000000 0.000000\n"

                            -- positions + 4
                            , "f 5/1 6/2 7/3\n"
                            , "f 5/1 7/3 8/4\n"
                            , "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"

                            -- positions + 8, uvs + 4
                            , "f 9//1 10//2 11//3\n"
                            , "f 9//1 11//3 12//4\n"
                            , "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "vt 0.000000 1.000000\n"
                            , "vt 1.000000 1.000000\n"
                            , "vt 1.000000 0.000000\n"
                            , "vt 0.000000 0.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"

                            -- positions + 12, uvs + 4, normals + 4
                            , "f 13/5/5 14/6/6 15/7/7\n"
                            , "f 13/5/5 15/7/7 16/8/8\n"
                            ]
                        )
        , Test.test "correctly offsets indices for lines and points" <|
            \_ ->
                Encode.encodeMultipart Length.inMeters
                    [ Encode.polylines
                        [ Polyline3d.fromVertices
                            [ Point3d.meters -4.5 4.5 0
                            , Point3d.meters 4.5 4.5 0
                            ]
                        , Polyline3d.fromVertices
                            [ Point3d.meters 4.5 -4.5 0
                            , Point3d.meters -4.5 -4.5 0
                            ]
                        ]
                    , Encode.points [ Point3d.meters -4.5 4.5 0, Point3d.meters 4.5 -4.5 0 ]
                    , Encode.points [ Point3d.meters 4.5 -4.5 0 ]
                    ]
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "l 1 2\n"
                            , "l 3 4\n"
                            , "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"

                            -- offset by 4
                            , "p 5\n"
                            , "p 6\n"
                            , "g\n"
                            , "v 4.500000 -4.500000 0.000000\n"

                            -- offset by 4 + 2 = 6
                            , "p 7\n"
                            ]
                        )
        ]


compact : Test
compact =
    Test.describe "compact"
        [ Test.test "correctly reindexes triangles" <|
            \_ ->
                Encode.encodeCompact Length.inMeters
                    [ Encode.triangles trianglesSquare ]
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "f 1 2 3\n"
                            , "f 1 3 4\n"
                            ]
                        )
        , Test.test "correctly reindexes faces" <|
            \_ ->
                Encode.encodeCompact Length.inMeters
                    [ Encode.faces suboptimalTexturedFacesSquare ]
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "f 1//1 2//1 3//1\n"
                            , "f 1//1 3//1 4//1\n"
                            ]
                        )
        , Test.test "correctly reindexes texturedTriangles" <|
            \_ ->
                Encode.encodeCompact Length.inMeters
                    [ Encode.texturedTriangles suboptimalTexturedFacesSquare ]
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "vt 0.000000 1.000000\n"
                            , "vt 1.000000 1.000000\n"
                            , "vt 1.000000 0.000000\n"
                            , "vt 0.000000 0.000000\n"
                            , "f 1/1 2/2 3/3\n"
                            , "f 1/1 3/3 4/4\n"
                            ]
                        )
        , Test.test "correctly reindexes texturedFaces" <|
            \_ ->
                Encode.encodeCompact Length.inMeters
                    [ Encode.texturedFaces suboptimalTexturedFacesSquare ]
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "vt 0.000000 1.000000\n"
                            , "vt 1.000000 1.000000\n"
                            , "vt 1.000000 0.000000\n"
                            , "vt 0.000000 0.000000\n"
                            , "vn 0.000000 0.000000 1.000000\n"
                            , "f 1/1/1 2/2/1 3/3/1\n"
                            , "f 1/1/1 3/3/1 4/4/1\n"
                            ]
                        )
        , Test.test "correctly reindexes lines" <|
            \_ ->
                Encode.encodeCompact Length.inMeters
                    [ Encode.polylines
                        [ Polyline3d.fromVertices
                            [ Point3d.meters -4.5 4.5 0
                            , Point3d.meters 4.5 4.5 0
                            ]
                        , Polyline3d.fromVertices
                            [ Point3d.meters 4.5 -4.5 0
                            , Point3d.meters -4.5 -4.5 0
                            ]
                        , Polyline3d.fromVertices
                            [ Point3d.meters -4.5 4.5 0
                            , Point3d.meters -4.5 -4.5 0
                            ]
                        ]
                    ]
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "l 1 2\n"
                            , "l 3 4\n"
                            , "l 1 4\n"
                            ]
                        )
        , Test.test "correctly reindexes points" <|
            \_ ->
                Encode.encodeCompact Length.inMeters
                    [ Encode.points
                        [ Point3d.meters -4.5 4.5 0
                        , Point3d.meters 4.5 4.5 0
                        , Point3d.meters 4.5 -4.5 0
                        , Point3d.meters -4.5 -4.5 0
                        , Point3d.meters -4.5 4.5 0
                        , Point3d.meters -4.5 -4.5 0
                        ]
                    ]
                    |> Expect.equal
                        (String.concat
                            [ "g\n"
                            , "v -4.500000 4.500000 0.000000\n"
                            , "v 4.500000 4.500000 0.000000\n"
                            , "v 4.500000 -4.500000 0.000000\n"
                            , "v -4.500000 -4.500000 0.000000\n"
                            , "p 1\n"
                            , "p 2\n"
                            , "p 3\n"
                            , "p 4\n"
                            , "p 1\n"
                            , "p 4\n"
                            ]
                        )
        , Test.test "skips empty polylines" <|
            \_ ->
                Encode.encodeCompact Length.inMeters
                    [ Encode.polylines [] ]
                    |> Expect.equal ""
        , Test.test "skips polylines without vertices" <|
            \_ ->
                Encode.encodeCompact Length.inMeters
                    [ Encode.polylines [ Polyline3d.fromVertices [] ] ]
                    |> Expect.equal ""
        , Test.test "skips empty meshes" <|
            \_ ->
                Encode.encodeCompact Length.inMeters
                    [ Encode.triangles TriangularMesh.empty ]
                    |> Expect.equal ""
        ]



-- FIXTURES


unsafeOptions : Options
unsafeOptions =
    { precision = 6
    , object = Just "Object Name"
    , groups = [ "Group 1", "Group\n 2" ]
    , material = Just "Test Material"
    }


trianglesSquare : TriangularMesh (Point3d Meters coords)
trianglesSquare =
    let
        vertices =
            Array.fromList
                [ Point3d.meters -4.5 4.5 0
                , Point3d.meters 4.5 4.5 0
                , Point3d.meters 4.5 -4.5 0
                , Point3d.meters -4.5 -4.5 0
                ]

        faceIndices =
            [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]
    in
    TriangularMesh.indexed vertices faceIndices


texturedFacesSquare : TriangularMesh { position : Point3d Meters coords, normal : Vector3d Unitless coords, uv : ( Float, Float ) }
texturedFacesSquare =
    let
        vertices =
            Array.fromList
                [ { position = Point3d.meters -4.5 4.5 0, uv = ( 0, 1 ), normal = Vector3d.unitless 0 0 1 }
                , { position = Point3d.meters 4.5 4.5 0, uv = ( 1, 1 ), normal = Vector3d.unitless 0 0 1 }
                , { position = Point3d.meters 4.5 -4.5 0, uv = ( 1, 0 ), normal = Vector3d.unitless 0 0 1 }
                , { position = Point3d.meters -4.5 -4.5 0, uv = ( 0, 0 ), normal = Vector3d.unitless 0 0 1 }
                ]

        faceIndices =
            [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]
    in
    TriangularMesh.indexed vertices faceIndices


suboptimalTexturedFacesSquare : TriangularMesh { position : Point3d Meters coords, normal : Vector3d Unitless coords, uv : ( Float, Float ) }
suboptimalTexturedFacesSquare =
    let
        vertices =
            Array.fromList
                [ { position = Point3d.meters -4.5 4.5 0, uv = ( 0, 1 ), normal = Vector3d.unitless 0 0 1 }
                , { position = Point3d.meters 4.5 4.5 0, uv = ( 1, 1 ), normal = Vector3d.unitless 0 0 1 }
                , { position = Point3d.meters 4.5 -4.5 0, uv = ( 1, 0 ), normal = Vector3d.unitless 0 0 1 }
                , { position = Point3d.meters -4.5 4.5 0, uv = ( 0, 1 ), normal = Vector3d.unitless 0 0 1 }
                , { position = Point3d.meters 4.5 -4.5 0, uv = ( 1, 0 ), normal = Vector3d.unitless 0 0 1 }
                , { position = Point3d.meters -4.5 -4.5 0, uv = ( 0, 0 ), normal = Vector3d.unitless 0 0 1 }
                ]

        faceIndices =
            [ ( 0, 1, 2 ), ( 3, 4, 5 ) ]
    in
    TriangularMesh.indexed vertices faceIndices


precisionTriangle : TriangularMesh (Point3d Meters coords)
precisionTriangle =
    let
        vertices =
            Array.fromList
                [ Point3d.meters -5.999999999 5.999999999 0.123456789
                , Point3d.meters -0.123456789 -1.123456789 0
                , Point3d.meters (0 / 0) (1 / 0) 1.23456789e-10
                ]
    in
    TriangularMesh.indexed vertices [ ( 2, 1, 0 ) ]
