module Encode exposing
    ( encode, Geometry
    , triangles, faces, texturedTriangles, texturedFaces, polylines, points
    , encodeMultipart, encodeCompact, Options, defaultOptions
    , trianglesWith, facesWith, texturedTrianglesWith, texturedFacesWith, polylinesWith, pointsWith
    )

{-| Turn different geometry into OBJ files.


# Encoding

@docs encode, Geometry


# Primitives

@docs triangles, faces, texturedTriangles, texturedFaces, polylines, points


# Advanced Encoding

@docs encodeMultipart, encodeCompact, Options, defaultOptions
@docs trianglesWith, facesWith, texturedTrianglesWith, texturedFacesWith, polylinesWith, pointsWith

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Length exposing (Length, Meters)
import Obj.Decode exposing (object)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import Quantity exposing (Unitless)
import String
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


{-| Encode geometry in the OBJ format, parametrized by the function,
that knows how to convert units.

    obj =
        encode Length.inMeters (triangles mesh)

To encode multipart files, control precision or add various metadata, see
[Advanced Encoding](#advanced-encoding).

-}
encode : (Length -> Float) -> Geometry -> String
encode units geometry =
    encodeMultipart units [ geometry ]


{-| Represents encoded geometry.
-}
type Geometry
    = Triangles Options Int (Array { px : Length, py : Length, pz : Length }) (List ( Int, Int, Int ))
    | Faces Options Int (Array { px : Length, py : Length, pz : Length, nx : Float, ny : Float, nz : Float }) (List ( Int, Int, Int ))
    | TexturedTriangles Options Int (Array { px : Length, py : Length, pz : Length, u : Float, v : Float }) (List ( Int, Int, Int ))
    | TexturedFaces Options Int (Array { px : Length, py : Length, pz : Length, nx : Float, ny : Float, nz : Float, u : Float, v : Float }) (List ( Int, Int, Int ))
    | Lines Options (List (List { px : Length, py : Length, pz : Length }))
    | Points Options (List { px : Length, py : Length, pz : Length })
    | Empty


{-| Encode positions.
-}
triangles : TriangularMesh (Point3d Meters coords) -> Geometry
triangles =
    trianglesWith defaultOptions


{-| Encode positions and normal vectors.
-}
faces : TriangularMesh { a | position : Point3d Meters coords, normal : Vector3d Unitless coords } -> Geometry
faces =
    facesWith defaultOptions


{-| Encode positions and [UV](https://learnopengl.com/Getting-started/Textures) (texture) coordinates.
-}
texturedTriangles : TriangularMesh { a | position : Point3d Meters coords, uv : ( Float, Float ) } -> Geometry
texturedTriangles =
    texturedTrianglesWith defaultOptions


{-| Encode positions, UV and normal vectors.
-}
texturedFaces : TriangularMesh { a | position : Point3d Meters coords, normal : Vector3d Unitless coords, uv : ( Float, Float ) } -> Geometry
texturedFaces =
    texturedFacesWith defaultOptions


{-| -}
polylines : List (Polyline3d Meters coords) -> Geometry
polylines =
    polylinesWith defaultOptions


{-| -}
points : List (Point3d Meters coords) -> Geometry
points =
    pointsWith defaultOptions


{-| Like `encode`, but for files made of multiple parts.

    multipartObj =
        encodeMultipart Length.inMeters
            [ triangles roofMesh
            , triangles wallsMesh
            ]

-}
encodeMultipart : (Length -> Float) -> List Geometry -> String
encodeMultipart units parts =
    encodeMultipartHelp units parts 1 1 1 ""


encodeMultipartHelp : (Length -> Float) -> List Geometry -> Int -> Int -> Int -> String -> String
encodeMultipartHelp units parts positionOffset uvOffset normalOffset result =
    case parts of
        (Triangles options size positions indices) :: remainingParts ->
            encodeMultipartHelp units
                remainingParts
                (positionOffset + size)
                uvOffset
                normalOffset
                (result
                    ++ encodeOptions options
                    ++ encodePositions (encodeFloat options.precision) units (Array.toList positions) ""
                    ++ encodeFaceIndices (encodePositionIndex positionOffset) indices ""
                )

        (Faces options size vertices indices) :: remainingParts ->
            encodeMultipartHelp units
                remainingParts
                (positionOffset + size)
                uvOffset
                (normalOffset + size)
                (result
                    ++ encodeOptions options
                    ++ encodePositions (encodeFloat options.precision) units (Array.toList vertices) ""
                    ++ encodeNormals (encodeFloat options.precision) (Array.toList vertices) ""
                    ++ encodeFaceIndices (encodeFacesIndex positionOffset normalOffset) indices ""
                )

        (TexturedTriangles options size vertices indices) :: remainingParts ->
            encodeMultipartHelp units
                remainingParts
                (positionOffset + size)
                (uvOffset + size)
                normalOffset
                (result
                    ++ encodeOptions options
                    ++ encodePositions (encodeFloat options.precision) units (Array.toList vertices) ""
                    ++ encodeUV (encodeFloat options.precision) (Array.toList vertices) ""
                    ++ encodeFaceIndices (encodeTexturedTrianglesIndex positionOffset uvOffset) indices ""
                )

        (TexturedFaces options size vertices indices) :: remainingParts ->
            encodeMultipartHelp units
                remainingParts
                (positionOffset + size)
                (uvOffset + size)
                (normalOffset + size)
                (result
                    ++ encodeOptions options
                    ++ encodePositions (encodeFloat options.precision) units (Array.toList vertices) ""
                    ++ encodeUV (encodeFloat options.precision) (Array.toList vertices) ""
                    ++ encodeNormals (encodeFloat options.precision) (Array.toList vertices) ""
                    ++ encodeFaceIndices (encodeTexturedFacesIndex positionOffset uvOffset normalOffset) indices ""
                )

        (Lines options lines) :: remainingParts ->
            case lines of
                firstLine :: remainingLines ->
                    case encodePolylines positionOffset (encodeFloat options.precision) units remainingLines firstLine 0 "" "l" of
                        ( 0, _ ) ->
                            encodeMultipartHelp units remainingParts positionOffset uvOffset normalOffset result

                        ( size, encodedLines ) ->
                            encodeMultipartHelp units
                                remainingParts
                                (positionOffset + size)
                                uvOffset
                                normalOffset
                                (result
                                    ++ encodeOptions options
                                    ++ encodedLines
                                )

                [] ->
                    encodeMultipartHelp units remainingParts positionOffset uvOffset normalOffset result

        (Points options positions) :: remainingParts ->
            case encodePointsIndices positionOffset positions 0 "" of
                ( 0, _ ) ->
                    encodeMultipartHelp units remainingParts positionOffset uvOffset normalOffset result

                ( size, pointsIndices ) ->
                    encodeMultipartHelp units
                        remainingParts
                        (positionOffset + size)
                        uvOffset
                        normalOffset
                        (result
                            ++ encodeOptions options
                            ++ encodePositions (encodeFloat options.precision) units positions ""
                            ++ pointsIndices
                        )

        Empty :: remainingParts ->
            encodeMultipartHelp units remainingParts positionOffset uvOffset normalOffset result

        [] ->
            result


{-| Like `encodeMultipart`, but reindexes triangular meshes.
This is slower, but produces smaller result.
-}
encodeCompact : (Length -> Float) -> List Geometry -> String
encodeCompact units parts =
    encodeCompactHelp units parts Dict.empty 1 Dict.empty 1 Dict.empty 1 ""


encodeCompactHelp : (Length -> Float) -> List Geometry -> Dict String Int -> Int -> Dict String Int -> Int -> Dict String Int -> Int -> String -> String
encodeCompactHelp units parts positionIndices positionOffset uvIndices uvOffset normalIndices normalOffset result =
    case parts of
        (TexturedFaces options _ vertices indices) :: remainingParts ->
            let
                encoded =
                    encodeCompactTexturedFaces (encodeFloat options.precision) units vertices indices 1 positionIndices positionOffset uvIndices uvOffset normalIndices normalOffset "" "" "" "f" ""
            in
            encodeCompactHelp units
                remainingParts
                encoded.positionIndices
                encoded.positionOffset
                encoded.uvIndices
                encoded.uvOffset
                encoded.normalIndices
                encoded.normalOffset
                (result
                    ++ encodeOptions options
                    ++ encoded.positions
                    ++ encoded.uvs
                    ++ encoded.normals
                    ++ encoded.faceIndices
                )

        Empty :: remainingParts ->
            encodeCompactHelp units remainingParts positionIndices positionOffset uvIndices uvOffset normalIndices normalOffset result

        _ :: remainingParts ->
            encodeCompactHelp units remainingParts positionIndices positionOffset uvIndices uvOffset normalIndices normalOffset result

        [] ->
            result


type T4 a b c d
    = T4 a b c d


encodeCompactTexturedFaces :
    (Float -> String)
    -> (Length -> Float)
    -> Array { px : Length, py : Length, pz : Length, nx : Float, ny : Float, nz : Float, u : Float, v : Float }
    -> List ( Int, Int, Int )
    -> Int
    -> Dict String Int
    -> Int
    -> Dict String Int
    -> Int
    -> Dict String Int
    -> Int
    -> String
    -> String
    -> String
    -> String
    -> String
    ->
        { positionIndices : Dict String Int
        , positions : String
        , positionOffset : Int
        , normalIndices : Dict String Int
        , normals : String
        , normalOffset : Int
        , uvIndices : Dict String Int
        , uvs : String
        , uvOffset : Int
        , faceIndices : String
        }
encodeCompactTexturedFaces encodeNumber units vertices indices indexOffset positionIndices positionOffset uvIndices uvOffset normalIndices normalOffset positions normals uvs currentFaceIndices faceIndices =
    case indices of
        ( i1, i2, i3 ) :: remainingIndices ->
            let
                index =
                    case indexOffset of
                        1 ->
                            i1

                        2 ->
                            i2

                        3 ->
                            i3

                        _ ->
                            -1
            in
            if index > -1 then
                case Array.get index vertices of
                    Just { px, py, pz, nx, ny, nz, u, v } ->
                        let
                            p =
                                "v " ++ encodeNumber (units px) ++ " " ++ encodeNumber (units py) ++ " " ++ encodeNumber (units pz) ++ "\n"

                            uv =
                                "vt " ++ encodeNumber u ++ " " ++ encodeNumber v ++ "\n"

                            n =
                                "vn " ++ encodeNumber nx ++ " " ++ encodeNumber ny ++ " " ++ encodeNumber nz ++ "\n"

                            (T4 newPositions pi newPositionOffset newPositionIndices) =
                                case Dict.get p positionIndices of
                                    Nothing ->
                                        T4 (positions ++ p) positionOffset (positionOffset + 1) (Dict.insert p positionOffset positionIndices)

                                    Just existingPositionIndex ->
                                        T4 positions existingPositionIndex positionOffset positionIndices

                            (T4 newUvs uvi newUvOffset newUvIndices) =
                                case Dict.get uv uvIndices of
                                    Nothing ->
                                        T4 (uvs ++ uv) uvOffset (uvOffset + 1) (Dict.insert uv uvOffset uvIndices)

                                    Just existingUvIndex ->
                                        T4 uvs existingUvIndex uvOffset uvIndices

                            (T4 newNormals ni newNormalOffset newNormalIndices) =
                                case Dict.get n normalIndices of
                                    Nothing ->
                                        T4 (normals ++ n) normalOffset (normalOffset + 1) (Dict.insert n normalOffset normalIndices)

                                    Just existingNormalIndex ->
                                        T4 normals existingNormalIndex normalOffset normalIndices
                        in
                        encodeCompactTexturedFaces encodeNumber
                            units
                            vertices
                            indices
                            (indexOffset + 1)
                            newPositionIndices
                            newPositionOffset
                            newUvIndices
                            newUvOffset
                            newNormalIndices
                            newNormalOffset
                            newPositions
                            newNormals
                            newUvs
                            (currentFaceIndices ++ " " ++ String.fromInt pi ++ "/" ++ String.fromInt uvi ++ "/" ++ String.fromInt ni)
                            faceIndices

                    Nothing ->
                        -- skip a face with out of bounds indices
                        encodeCompactTexturedFaces encodeNumber units vertices indices (indexOffset + 1) positionIndices positionOffset uvIndices uvOffset normalIndices normalOffset positions normals uvs "f" faceIndices

            else
                encodeCompactTexturedFaces encodeNumber units vertices remainingIndices 1 positionIndices positionOffset uvIndices uvOffset normalIndices normalOffset positions normals uvs "f" (faceIndices ++ currentFaceIndices ++ "\n")

        [] ->
            { positionIndices = positionIndices
            , positions = positions
            , positionOffset = positionOffset
            , normalIndices = normalIndices
            , normals = normals
            , normalOffset = normalOffset
            , uvIndices = uvIndices
            , uvs = uvs
            , uvOffset = uvOffset
            , faceIndices = faceIndices
            }


{-| Set decimal precision for geometry and label it with object,
groups or material. May be useful for labeling parts in a multipart OBJ file.

    multipartObj =
        encodeMultipart Length.inMeters
            [ trianglesWith { defaultOptions | object = Just "roof" } roofMesh
            , trianglesWith { defaultOptions | object = Just "walls" } wallsMesh
            ]

Note: the precision is clamped between 1 and 10, string options are stripped of whitespace.

-}
type alias Options =
    { precision : Int
    , object : Maybe String
    , groups : List String
    , material : Maybe String
    }


{-| Default options for geometry.

    defaultOptions =
        { precision = 6
        , object = Nothing
        , groups = []
        , material = Nothing
        }

    triangles =
        trianglesWith defaultOptions

-}
defaultOptions : Options
defaultOptions =
    { precision = 6
    , object = Nothing
    , groups = []
    , material = Nothing
    }


{-| -}
trianglesWith : Options -> TriangularMesh (Point3d Meters coords) -> Geometry
trianglesWith options mesh =
    let
        vertices =
            TriangularMesh.vertices mesh
    in
    case ( Array.length vertices, TriangularMesh.faceIndices mesh ) of
        ( 0, _ ) ->
            Empty

        ( _, [] ) ->
            Empty

        ( size, indices ) ->
            Triangles options
                size
                (Array.map positionToRecord vertices)
                indices


{-| -}
facesWith : Options -> TriangularMesh { a | position : Point3d Meters coords, normal : Vector3d Unitless coords } -> Geometry
facesWith options mesh =
    let
        vertices =
            TriangularMesh.vertices mesh

        vertexToRecord { position, normal } =
            { px = Point3d.xCoordinate position
            , py = Point3d.yCoordinate position
            , pz = Point3d.zCoordinate position
            , nx = Quantity.toFloat (Vector3d.xComponent normal)
            , ny = Quantity.toFloat (Vector3d.yComponent normal)
            , nz = Quantity.toFloat (Vector3d.zComponent normal)
            }
    in
    case ( Array.length vertices, TriangularMesh.faceIndices mesh ) of
        ( 0, _ ) ->
            Empty

        ( _, [] ) ->
            Empty

        ( size, indices ) ->
            Faces options
                size
                (Array.map vertexToRecord vertices)
                indices


{-| -}
texturedTrianglesWith : Options -> TriangularMesh { a | position : Point3d Meters coords, uv : ( Float, Float ) } -> Geometry
texturedTrianglesWith options mesh =
    let
        vertices =
            TriangularMesh.vertices mesh

        vertexToRecord { position, uv } =
            { px = Point3d.xCoordinate position
            , py = Point3d.yCoordinate position
            , pz = Point3d.zCoordinate position
            , u = Tuple.first uv
            , v = Tuple.second uv
            }
    in
    case ( Array.length vertices, TriangularMesh.faceIndices mesh ) of
        ( 0, _ ) ->
            Empty

        ( _, [] ) ->
            Empty

        ( size, indices ) ->
            TexturedTriangles options
                size
                (Array.map vertexToRecord vertices)
                indices


{-| -}
texturedFacesWith : Options -> TriangularMesh { a | position : Point3d Meters coords, normal : Vector3d Unitless coords, uv : ( Float, Float ) } -> Geometry
texturedFacesWith options mesh =
    let
        vertices =
            TriangularMesh.vertices mesh

        vertexToRecord { position, normal, uv } =
            { px = Point3d.xCoordinate position
            , py = Point3d.yCoordinate position
            , pz = Point3d.zCoordinate position
            , nx = Quantity.toFloat (Vector3d.xComponent normal)
            , ny = Quantity.toFloat (Vector3d.yComponent normal)
            , nz = Quantity.toFloat (Vector3d.zComponent normal)
            , u = Tuple.first uv
            , v = Tuple.second uv
            }
    in
    case ( Array.length vertices, TriangularMesh.faceIndices mesh ) of
        ( 0, _ ) ->
            Empty

        ( _, [] ) ->
            Empty

        ( size, indices ) ->
            TexturedFaces options
                size
                (Array.map vertexToRecord vertices)
                indices


{-| -}
polylinesWith : Options -> List (Polyline3d Meters coords) -> Geometry
polylinesWith options lines =
    Lines options
        (List.map (Polyline3d.vertices >> List.map positionToRecord) lines)


{-| -}
pointsWith : Options -> List (Point3d Meters coords) -> Geometry
pointsWith options pts =
    Points options (List.map positionToRecord pts)


positionToRecord : Point3d Meters coordinates -> { px : Length, py : Length, pz : Length }
positionToRecord position =
    { px = Point3d.xCoordinate position
    , py = Point3d.yCoordinate position
    , pz = Point3d.zCoordinate position
    }


encodeOptions : Options -> String
encodeOptions { groups, object, material } =
    String.concat
        [ case object of
            Just obj ->
                "o " ++ safeString obj ++ "\n"

            Nothing ->
                ""
        , case groups of
            [] ->
                "g\n"

            grps ->
                "g " ++ String.join " " (List.map safeString grps) ++ "\n"
        , case material of
            Just mtl ->
                "usemtl " ++ safeString mtl ++ "\n"

            Nothing ->
                ""
        ]


safeString : String -> String
safeString string =
    String.replace "\t" "" (String.replace "\u{000D}" "" (String.replace "\n" "" (String.replace " " "" string)))


encodePositions : (Float -> String) -> (Length -> Float) -> List { a | px : Length, py : Length, pz : Length } -> String -> String
encodePositions encodeNumber units positions result =
    case positions of
        { px, py, pz } :: remainingPositions ->
            encodePositions
                encodeNumber
                units
                remainingPositions
                (result
                    ++ "v "
                    ++ encodeNumber (units px)
                    ++ " "
                    ++ encodeNumber (units py)
                    ++ " "
                    ++ encodeNumber (units pz)
                    ++ "\n"
                )

        [] ->
            result


encodeNormals : (Float -> String) -> List { a | nx : Float, ny : Float, nz : Float } -> String -> String
encodeNormals encodeNumber normals result =
    case normals of
        { nx, ny, nz } :: remainingNormals ->
            encodeNormals
                encodeNumber
                remainingNormals
                (result
                    ++ "vn "
                    ++ encodeNumber nx
                    ++ " "
                    ++ encodeNumber ny
                    ++ " "
                    ++ encodeNumber nz
                    ++ "\n"
                )

        [] ->
            result


encodeUV : (Float -> String) -> List { a | u : Float, v : Float } -> String -> String
encodeUV encodeNumber uv result =
    case uv of
        { u, v } :: remainingUV ->
            encodeUV
                encodeNumber
                remainingUV
                (result
                    ++ "vt "
                    ++ encodeNumber u
                    ++ " "
                    ++ encodeNumber v
                    ++ "\n"
                )

        [] ->
            result


encodePositionIndex : Int -> Int -> String
encodePositionIndex positionOffset index =
    String.fromInt (positionOffset + index)


encodeFacesIndex : Int -> Int -> Int -> String
encodeFacesIndex positionOffset normalOffset index =
    String.fromInt (positionOffset + index)
        ++ "//"
        ++ String.fromInt (normalOffset + index)


encodeTexturedTrianglesIndex : Int -> Int -> Int -> String
encodeTexturedTrianglesIndex positionOffset uvOffset index =
    String.fromInt (positionOffset + index)
        ++ "/"
        ++ String.fromInt (uvOffset + index)


encodeTexturedFacesIndex : Int -> Int -> Int -> Int -> String
encodeTexturedFacesIndex positionOffset uvOffset normalOffset index =
    String.fromInt (positionOffset + index)
        ++ "/"
        ++ String.fromInt (uvOffset + index)
        ++ "/"
        ++ String.fromInt (normalOffset + index)


encodeFaceIndices : (Int -> String) -> List ( Int, Int, Int ) -> String -> String
encodeFaceIndices encodeIndex indices result =
    case indices of
        ( i1, i2, i3 ) :: remainingIndices ->
            encodeFaceIndices
                encodeIndex
                remainingIndices
                (result
                    ++ "f "
                    ++ encodeIndex i1
                    ++ " "
                    ++ encodeIndex i2
                    ++ " "
                    ++ encodeIndex i3
                    ++ "\n"
                )

        [] ->
            result


encodePolylines : Int -> (Float -> String) -> (Length -> Float) -> List (List { px : Length, py : Length, pz : Length }) -> List { px : Length, py : Length, pz : Length } -> Int -> String -> String -> ( Int, String )
encodePolylines positionOffset encodeNumber units lines positions count resultVertices resultIndices =
    case positions of
        { px, py, pz } :: remainingPositions ->
            encodePolylines
                positionOffset
                encodeNumber
                units
                lines
                remainingPositions
                (count + 1)
                (resultVertices
                    ++ "v "
                    ++ encodeNumber (units px)
                    ++ " "
                    ++ encodeNumber (units py)
                    ++ " "
                    ++ encodeNumber (units pz)
                    ++ "\n"
                )
                (resultIndices ++ " " ++ String.fromInt (positionOffset + count))

        [] ->
            case lines of
                [] ->
                    ( count, resultVertices ++ resultIndices ++ "\n" )

                [] :: remainingLines ->
                    encodePolylines positionOffset encodeNumber units remainingLines [] count resultVertices resultIndices

                nextLine :: remainingLines ->
                    encodePolylines positionOffset
                        encodeNumber
                        units
                        remainingLines
                        nextLine
                        count
                        resultVertices
                        (resultIndices ++ "\nl")


encodePointsIndices : Int -> List position -> Int -> String -> ( Int, String )
encodePointsIndices positionsOffset pts count result =
    case pts of
        _ :: remainingPoints ->
            encodePointsIndices
                positionsOffset
                remainingPoints
                (count + 1)
                (result
                    ++ "p "
                    ++ String.fromInt (positionsOffset + count)
                    ++ "\n"
                )

        [] ->
            ( count, result )


encodeFloat : Int -> Float -> String
encodeFloat precision =
    let
        n =
            clamp 1 10 precision

        decimals =
            toFloat (10 ^ n)
    in
    \float ->
        let
            absolute =
                abs float

            integer =
                truncate absolute

            fraction =
                round ((1 + absolute - toFloat integer) * decimals)

            fractionStr =
                String.dropLeft 1 (String.fromInt fraction)
        in
        if isNaN (float - float) then
            String.padRight (n + 2) '0' "0."

        else if float >= 0 then
            String.fromInt integer ++ "." ++ fractionStr

        else
            "-" ++ String.fromInt integer ++ "." ++ fractionStr