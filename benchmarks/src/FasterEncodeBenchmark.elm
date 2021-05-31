module FasterEncodeBenchmark exposing (main)

import Array
import Benchmark
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Encode
import Length exposing (Meters)
import Obj.Encode
import Point3d exposing (Point3d)
import Quantity exposing (Unitless)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


main : BenchmarkProgram
main =
    program <|
        Benchmark.compare "encode"
            "old texturedTriangles"
            (\_ -> Encode.encode Length.inMeters (Encode.texturedTriangles mesh))
            "texturedTriangles"
            (\_ -> Obj.Encode.encode Length.inMeters (Obj.Encode.texturedTriangles mesh))


mesh : TriangularMesh { position : Point3d Meters coords, normal : Vector3d Unitless coords, uv : ( Float, Float ) }
mesh =
    let
        vertices =
            Array.fromList
                [ { position = Point3d.meters -4.5 4.5 0, uv = ( 0, 1 ), normal = Vector3d.unitless 0 0 1 }
                , { position = Point3d.meters 4.5 4.5 0, uv = ( 1, 1 ), normal = Vector3d.unitless 0 0 1 }
                , { position = Point3d.meters 4.5 -4.5 0, uv = ( 1, 0 ), normal = Vector3d.unitless 0 0 1 }
                , { position = Point3d.meters -3.5 3.5 0, uv = ( 0, 1 ), normal = Vector3d.unitless 0 0 1 }
                , { position = Point3d.meters 3.5 -3.5 0, uv = ( 1, 0 ), normal = Vector3d.unitless 0 0 1 }
                , { position = Point3d.meters -3.5 -3.5 0, uv = ( 0, 0 ), normal = Vector3d.unitless 0 0 1 }
                , { position = Point3d.meters -4.5 4.5 0, uv = ( 0.5, 1 ), normal = Vector3d.unitless 1 0 0 }
                , { position = Point3d.meters 4.5 -4.5 0, uv = ( 1, 0.5 ), normal = Vector3d.unitless 1 0 0 }
                , { position = Point3d.meters -4.5 -4.5 0, uv = ( 0.5, 0.5 ), normal = Vector3d.unitless 0 1 0 }
                ]

        faceIndices =
            [ ( 0, 1, 2 ), ( 3, 4, 5 ), ( 6, 7, 8 ) ]
    in
    TriangularMesh.indexed vertices faceIndices
