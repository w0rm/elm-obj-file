module Obj.Encode exposing
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
    | Faces Options Int (Array { px : Length, py : Length, pz : Length, normal : String }) (List ( Int, Int, Int ))
    | TexturedTriangles Options Int (Array { px : Length, py : Length, pz : Length, uv : String }) (List ( Int, Int, Int ))
    | TexturedFaces Options Int (Array { px : Length, py : Length, pz : Length, normal : String, uv : String }) (List ( Int, Int, Int ))
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
                    ++ encodePositions (encodeLength options.precision units) (Array.toList positions) ""
                    ++ encodeFaceIndices (encodePositionIndex positionOffset) indices ""
                    ++ ""
                )

        (Faces options size vertices indices) :: remainingParts ->
            let
                verticesList =
                    Array.toList vertices
            in
            encodeMultipartHelp units
                remainingParts
                (positionOffset + size)
                uvOffset
                (normalOffset + size)
                (result
                    ++ encodeOptions options
                    ++ encodePositions (encodeLength options.precision units) verticesList ""
                    ++ encodeNormals verticesList ""
                    ++ encodeFaceIndices (encodeFacesIndex positionOffset normalOffset) indices ""
                    ++ ""
                )

        (TexturedTriangles options size vertices indices) :: remainingParts ->
            let
                verticesList =
                    Array.toList vertices
            in
            encodeMultipartHelp units
                remainingParts
                (positionOffset + size)
                (uvOffset + size)
                normalOffset
                (result
                    ++ encodeOptions options
                    ++ encodePositions (encodeLength options.precision units) verticesList ""
                    ++ encodeUvs verticesList ""
                    ++ encodeFaceIndices (encodeTexturedTrianglesIndex positionOffset uvOffset) indices ""
                    ++ ""
                )

        (TexturedFaces options size vertices indices) :: remainingParts ->
            let
                verticesList =
                    Array.toList vertices
            in
            encodeMultipartHelp units
                remainingParts
                (positionOffset + size)
                (uvOffset + size)
                (normalOffset + size)
                (result
                    ++ encodeOptions options
                    ++ encodePositions (encodeLength options.precision units) verticesList ""
                    ++ encodeUvs verticesList ""
                    ++ encodeNormals verticesList ""
                    ++ encodeFaceIndices (encodeTexturedFacesIndex positionOffset uvOffset normalOffset) indices ""
                    ++ ""
                )

        (Lines options lines) :: remainingParts ->
            case lines of
                firstLine :: remainingLines ->
                    case encodePolylines (encodeLength options.precision units) remainingLines firstLine positionOffset "" "l" of
                        ( _, "l\n" ) ->
                            encodeMultipartHelp units remainingParts positionOffset uvOffset normalOffset result

                        ( newPositionOffset, encodedLines ) ->
                            encodeMultipartHelp units
                                remainingParts
                                newPositionOffset
                                uvOffset
                                normalOffset
                                (result ++ encodeOptions options ++ encodedLines ++ "")

                [] ->
                    encodeMultipartHelp units remainingParts positionOffset uvOffset normalOffset result

        (Points options positions) :: remainingParts ->
            let
                ( size, pointsIndices ) =
                    encodePointsIndices positionOffset positions 0 ""
            in
            encodeMultipartHelp units
                remainingParts
                (positionOffset + size)
                uvOffset
                normalOffset
                (result
                    ++ encodeOptions options
                    ++ encodePositions (encodeLength options.precision units) positions ""
                    ++ pointsIndices
                    ++ ""
                )

        Empty :: remainingParts ->
            encodeMultipartHelp units remainingParts positionOffset uvOffset normalOffset result

        [] ->
            result


{-| Like `encodeMultipart`, but reindexes triangular meshes.
This is slower, but produces smaller result, because it
deduplicates stored data.
-}
encodeCompact : (Length -> Float) -> List Geometry -> String
encodeCompact units parts =
    encodeCompactHelp units parts Dict.empty 1 1 1 ""


encodeCompactHelp : (Length -> Float) -> List Geometry -> Dict String Int -> Int -> Int -> Int -> String -> String
encodeCompactHelp units parts indicesMap positionOffset uvOffset normalOffset result =
    case parts of
        (Triangles options _ vertices indices) :: remainingParts ->
            let
                encoded =
                    encodeCompactTriangles (encodeLength options.precision units) vertices indices 1 indicesMap positionOffset "" "f" ""
            in
            encodeCompactHelp units
                remainingParts
                encoded.indicesMap
                encoded.positionOffset
                uvOffset
                normalOffset
                (result ++ encodeOptions options ++ encoded.result ++ "")

        (Faces options _ vertices indices) :: remainingParts ->
            let
                encoded =
                    encodeCompactFaces (encodeLength options.precision units) vertices indices 1 indicesMap positionOffset normalOffset "" "" "f" ""
            in
            encodeCompactHelp units
                remainingParts
                encoded.indicesMap
                encoded.positionOffset
                uvOffset
                encoded.normalOffset
                (result ++ encodeOptions options ++ encoded.result ++ "")

        (TexturedTriangles options _ vertices indices) :: remainingParts ->
            let
                encoded =
                    encodeCompactTexturedTriangles (encodeLength options.precision units) vertices indices 1 indicesMap positionOffset uvOffset "" "" "f" ""
            in
            encodeCompactHelp units
                remainingParts
                encoded.indicesMap
                encoded.positionOffset
                encoded.uvOffset
                normalOffset
                (result ++ encodeOptions options ++ encoded.result ++ "")

        (TexturedFaces options _ vertices indices) :: remainingParts ->
            let
                encoded =
                    encodeCompactTexturedFaces (encodeLength options.precision units) vertices indices 1 indicesMap positionOffset uvOffset normalOffset "" "" "" "f" ""
            in
            encodeCompactHelp units
                remainingParts
                encoded.indicesMap
                encoded.positionOffset
                encoded.uvOffset
                encoded.normalOffset
                (result ++ encodeOptions options ++ encoded.result ++ "")

        (Lines options lines) :: remainingParts ->
            case lines of
                firstLine :: remainingLines ->
                    let
                        encoded =
                            encodeCompactPolylines (encodeLength options.precision units) remainingLines firstLine indicesMap positionOffset "" "l"
                    in
                    if encoded.result == "l\n" then
                        encodeCompactHelp units remainingParts indicesMap positionOffset uvOffset normalOffset result

                    else
                        encodeCompactHelp units
                            remainingParts
                            encoded.indicesMap
                            encoded.positionOffset
                            uvOffset
                            normalOffset
                            (result ++ encodeOptions options ++ encoded.result ++ "")

                [] ->
                    encodeCompactHelp units remainingParts indicesMap positionOffset uvOffset normalOffset result

        (Points options positions) :: remainingParts ->
            let
                encoded =
                    encodeCompactPoints (encodeLength options.precision units) positions indicesMap positionOffset "" ""
            in
            encodeCompactHelp units
                remainingParts
                encoded.indicesMap
                encoded.positionOffset
                uvOffset
                normalOffset
                (result ++ encodeOptions options ++ encoded.result ++ "")

        Empty :: remainingParts ->
            encodeCompactHelp units remainingParts indicesMap positionOffset uvOffset normalOffset result

        [] ->
            result


encodeCompactPolylines :
    (Length -> String)
    -> List (List { px : Length, py : Length, pz : Length })
    -> List { px : Length, py : Length, pz : Length }
    -> Dict String Int
    -> Int
    -> String
    -> String
    ->
        { indicesMap : Dict String Int
        , positionOffset : Int
        , result : String
        }
encodeCompactPolylines lengthToString lines vertices indicesMap positionOffset resultVertices resultIndices =
    case vertices of
        { px, py, pz } :: remainingVertices ->
            let
                p =
                    "v " ++ lengthToString px ++ " " ++ lengthToString py ++ " " ++ lengthToString pz ++ "\n"
            in
            case Dict.get p indicesMap of
                Nothing ->
                    encodeCompactPolylines lengthToString
                        lines
                        remainingVertices
                        (Dict.insert p positionOffset indicesMap)
                        (positionOffset + 1)
                        (resultVertices ++ p ++ "")
                        (resultIndices ++ " " ++ String.fromInt positionOffset)

                Just existingPositionIndex ->
                    encodeCompactPolylines lengthToString
                        lines
                        remainingVertices
                        indicesMap
                        positionOffset
                        resultVertices
                        (resultIndices ++ " " ++ String.fromInt existingPositionIndex)

        [] ->
            case lines of
                [] ->
                    { indicesMap = indicesMap
                    , positionOffset = positionOffset
                    , result = resultVertices ++ resultIndices ++ "\n"
                    }

                [] :: remainingLines ->
                    encodeCompactPolylines
                        lengthToString
                        remainingLines
                        []
                        indicesMap
                        positionOffset
                        resultVertices
                        resultIndices

                nextLine :: remainingLines ->
                    encodeCompactPolylines
                        lengthToString
                        remainingLines
                        nextLine
                        indicesMap
                        positionOffset
                        resultVertices
                        (resultIndices ++ "\nl")


encodeCompactPoints :
    (Length -> String)
    -> List { px : Length, py : Length, pz : Length }
    -> Dict String Int
    -> Int
    -> String
    -> String
    ->
        { indicesMap : Dict String Int
        , positionOffset : Int
        , result : String
        }
encodeCompactPoints lengthToString vertices indicesMap positionOffset positions pointIndices =
    case vertices of
        { px, py, pz } :: remainingVertices ->
            let
                p =
                    "v " ++ lengthToString px ++ " " ++ lengthToString py ++ " " ++ lengthToString pz ++ "\n"
            in
            case Dict.get p indicesMap of
                Nothing ->
                    encodeCompactPoints lengthToString
                        remainingVertices
                        (Dict.insert p positionOffset indicesMap)
                        (positionOffset + 1)
                        (positions ++ p ++ "")
                        (pointIndices ++ "p " ++ String.fromInt positionOffset ++ "\n")

                Just existingPositionIndex ->
                    encodeCompactPoints lengthToString
                        remainingVertices
                        indicesMap
                        positionOffset
                        positions
                        (pointIndices ++ "p " ++ String.fromInt existingPositionIndex ++ "\n")

        [] ->
            { indicesMap = indicesMap
            , positionOffset = positionOffset
            , result = positions ++ pointIndices ++ ""
            }


encodeCompactTriangles :
    (Length -> String)
    -> Array { px : Length, py : Length, pz : Length }
    -> List ( Int, Int, Int )
    -> Int
    -> Dict String Int
    -> Int
    -> String
    -> String
    -> String
    ->
        { indicesMap : Dict String Int
        , positionOffset : Int
        , result : String
        }
encodeCompactTriangles lengthToString vertices indices indexOffset indicesMap positionOffset positions currentFaceIndices faceIndices =
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
                    Just { px, py, pz } ->
                        let
                            p =
                                "v " ++ lengthToString px ++ " " ++ lengthToString py ++ " " ++ lengthToString pz ++ "\n"
                        in
                        case Dict.get p indicesMap of
                            Nothing ->
                                encodeCompactTriangles lengthToString
                                    vertices
                                    indices
                                    (indexOffset + 1)
                                    (Dict.insert p positionOffset indicesMap)
                                    (positionOffset + 1)
                                    (positions ++ p ++ "")
                                    (currentFaceIndices ++ " " ++ String.fromInt positionOffset)
                                    faceIndices

                            Just existingPositionIndex ->
                                encodeCompactTriangles lengthToString
                                    vertices
                                    indices
                                    (indexOffset + 1)
                                    indicesMap
                                    positionOffset
                                    positions
                                    (currentFaceIndices ++ " " ++ String.fromInt existingPositionIndex)
                                    faceIndices

                    Nothing ->
                        -- skip a face with out of bounds indices
                        -- but actually this should never happen because it is not possible
                        -- to construct such TriangularMesh
                        encodeCompactTriangles lengthToString vertices indices (indexOffset + 1) indicesMap positionOffset positions "f" faceIndices

            else
                encodeCompactTriangles lengthToString vertices remainingIndices 1 indicesMap positionOffset positions "f" (faceIndices ++ currentFaceIndices ++ "\n")

        [] ->
            { indicesMap = indicesMap
            , positionOffset = positionOffset
            , result = positions ++ faceIndices ++ ""
            }


type T4 a b c d
    = T4 a b c d


encodeCompactFaces :
    (Length -> String)
    -> Array { px : Length, py : Length, pz : Length, normal : String }
    -> List ( Int, Int, Int )
    -> Int
    -> Dict String Int
    -> Int
    -> Int
    -> String
    -> String
    -> String
    -> String
    ->
        { indicesMap : Dict String Int
        , positionOffset : Int
        , normalOffset : Int
        , result : String
        }
encodeCompactFaces lengthToString vertices indices indexOffset indicesMap positionOffset normalOffset positions normals currentFaceIndices faceIndices =
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
                    Just { px, py, pz, normal } ->
                        let
                            p =
                                "v " ++ lengthToString px ++ " " ++ lengthToString py ++ " " ++ lengthToString pz ++ "\n"

                            (T4 newPositions pi newPositionOffset indicesMap1) =
                                case Dict.get p indicesMap of
                                    Nothing ->
                                        T4 (positions ++ p ++ "") positionOffset (positionOffset + 1) (Dict.insert p positionOffset indicesMap)

                                    Just existingPositionIndex ->
                                        T4 positions existingPositionIndex positionOffset indicesMap

                            (T4 newNormals ni newNormalOffset indicesMap2) =
                                case Dict.get normal indicesMap1 of
                                    Nothing ->
                                        T4 (normals ++ normal ++ "") normalOffset (normalOffset + 1) (Dict.insert normal normalOffset indicesMap1)

                                    Just existingNormalIndex ->
                                        T4 normals existingNormalIndex normalOffset indicesMap1
                        in
                        encodeCompactFaces lengthToString
                            vertices
                            indices
                            (indexOffset + 1)
                            indicesMap2
                            newPositionOffset
                            newNormalOffset
                            newPositions
                            newNormals
                            (currentFaceIndices ++ " " ++ String.fromInt pi ++ "//" ++ String.fromInt ni)
                            faceIndices

                    Nothing ->
                        -- skip a face with out of bounds indices
                        encodeCompactFaces lengthToString vertices indices (indexOffset + 1) indicesMap positionOffset normalOffset positions normals "f" faceIndices

            else
                encodeCompactFaces lengthToString vertices remainingIndices 1 indicesMap positionOffset normalOffset positions normals "f" (faceIndices ++ currentFaceIndices ++ "\n")

        [] ->
            { indicesMap = indicesMap
            , positionOffset = positionOffset
            , normalOffset = normalOffset
            , result = positions ++ normals ++ faceIndices ++ ""
            }


encodeCompactTexturedTriangles :
    (Length -> String)
    -> Array { px : Length, py : Length, pz : Length, uv : String }
    -> List ( Int, Int, Int )
    -> Int
    -> Dict String Int
    -> Int
    -> Int
    -> String
    -> String
    -> String
    -> String
    ->
        { indicesMap : Dict String Int
        , positionOffset : Int
        , uvOffset : Int
        , result : String
        }
encodeCompactTexturedTriangles lengthToString vertices indices indexOffset indicesMap positionOffset uvOffset positions uvs currentFaceIndices faceIndices =
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
                    Just { px, py, pz, uv } ->
                        let
                            p =
                                "v " ++ lengthToString px ++ " " ++ lengthToString py ++ " " ++ lengthToString pz ++ "\n"

                            (T4 newPositions pi newPositionOffset indicesMap1) =
                                case Dict.get p indicesMap of
                                    Nothing ->
                                        T4 (positions ++ p ++ "") positionOffset (positionOffset + 1) (Dict.insert p positionOffset indicesMap)

                                    Just existingPositionIndex ->
                                        T4 positions existingPositionIndex positionOffset indicesMap

                            (T4 newUvs uvi newUvOffset indicesMap2) =
                                case Dict.get uv indicesMap1 of
                                    Nothing ->
                                        T4 (uvs ++ uv ++ "") uvOffset (uvOffset + 1) (Dict.insert uv uvOffset indicesMap1)

                                    Just existingUvIndex ->
                                        T4 uvs existingUvIndex uvOffset indicesMap1
                        in
                        encodeCompactTexturedTriangles lengthToString
                            vertices
                            indices
                            (indexOffset + 1)
                            indicesMap2
                            newPositionOffset
                            newUvOffset
                            newPositions
                            newUvs
                            (currentFaceIndices ++ " " ++ String.fromInt pi ++ "/" ++ String.fromInt uvi)
                            faceIndices

                    Nothing ->
                        -- skip a face with out of bounds indices
                        encodeCompactTexturedTriangles lengthToString vertices indices (indexOffset + 1) indicesMap positionOffset uvOffset positions uvs "f" faceIndices

            else
                encodeCompactTexturedTriangles lengthToString vertices remainingIndices 1 indicesMap positionOffset uvOffset positions uvs "f" (faceIndices ++ currentFaceIndices ++ "\n")

        [] ->
            { indicesMap = indicesMap
            , positionOffset = positionOffset
            , uvOffset = uvOffset
            , result = positions ++ uvs ++ faceIndices ++ ""
            }


encodeCompactTexturedFaces :
    (Length -> String)
    -> Array { px : Length, py : Length, pz : Length, normal : String, uv : String }
    -> List ( Int, Int, Int )
    -> Int
    -> Dict String Int
    -> Int
    -> Int
    -> Int
    -> String
    -> String
    -> String
    -> String
    -> String
    ->
        { indicesMap : Dict String Int
        , positionOffset : Int
        , normalOffset : Int
        , uvOffset : Int
        , result : String
        }
encodeCompactTexturedFaces lengthToString vertices indices indexOffset indicesMap positionOffset uvOffset normalOffset positions normals uvs currentFaceIndices faceIndices =
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
                    Just { px, py, pz, normal, uv } ->
                        let
                            p =
                                "v " ++ lengthToString px ++ " " ++ lengthToString py ++ " " ++ lengthToString pz ++ "\n"

                            (T4 newPositions pi newPositionOffset indicesMap1) =
                                case Dict.get p indicesMap of
                                    Nothing ->
                                        T4 (positions ++ p ++ "") positionOffset (positionOffset + 1) (Dict.insert p positionOffset indicesMap)

                                    Just existingPositionIndex ->
                                        T4 positions existingPositionIndex positionOffset indicesMap

                            (T4 newUvs uvi newUvOffset indicesMap2) =
                                case Dict.get uv indicesMap1 of
                                    Nothing ->
                                        T4 (uvs ++ uv ++ "") uvOffset (uvOffset + 1) (Dict.insert uv uvOffset indicesMap1)

                                    Just existingUvIndex ->
                                        T4 uvs existingUvIndex uvOffset indicesMap1

                            (T4 newNormals ni newNormalOffset indicesMap3) =
                                case Dict.get normal indicesMap2 of
                                    Nothing ->
                                        T4 (normals ++ normal ++ "") normalOffset (normalOffset + 1) (Dict.insert normal normalOffset indicesMap2)

                                    Just existingNormalIndex ->
                                        T4 normals existingNormalIndex normalOffset indicesMap2
                        in
                        encodeCompactTexturedFaces lengthToString
                            vertices
                            indices
                            (indexOffset + 1)
                            indicesMap3
                            newPositionOffset
                            newUvOffset
                            newNormalOffset
                            newPositions
                            newNormals
                            newUvs
                            (currentFaceIndices ++ " " ++ String.fromInt pi ++ "/" ++ String.fromInt uvi ++ "/" ++ String.fromInt ni)
                            faceIndices

                    Nothing ->
                        -- skip a face with out of bounds indices
                        encodeCompactTexturedFaces lengthToString vertices indices (indexOffset + 1) indicesMap positionOffset uvOffset normalOffset positions normals uvs "f" faceIndices

            else
                encodeCompactTexturedFaces lengthToString vertices remainingIndices 1 indicesMap positionOffset uvOffset normalOffset positions normals uvs "f" (faceIndices ++ currentFaceIndices ++ "\n")

        [] ->
            { indicesMap = indicesMap
            , positionOffset = positionOffset
            , normalOffset = normalOffset
            , uvOffset = uvOffset
            , result = positions ++ uvs ++ normals ++ faceIndices ++ ""
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

        encodeNumber =
            encodeFloat options.precision

        vertexToRecord { position, normal } =
            { px = Point3d.xCoordinate position
            , py = Point3d.yCoordinate position
            , pz = Point3d.zCoordinate position
            , normal =
                "vn "
                    ++ encodeNumber (Quantity.toFloat (Vector3d.xComponent normal))
                    ++ " "
                    ++ encodeNumber (Quantity.toFloat (Vector3d.yComponent normal))
                    ++ " "
                    ++ encodeNumber (Quantity.toFloat (Vector3d.zComponent normal))
                    ++ "\n"
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

        encodeNumber =
            encodeFloat options.precision

        vertexToRecord { position, uv } =
            { px = Point3d.xCoordinate position
            , py = Point3d.yCoordinate position
            , pz = Point3d.zCoordinate position
            , uv =
                "vt "
                    ++ encodeNumber (Tuple.first uv)
                    ++ " "
                    ++ encodeNumber (Tuple.second uv)
                    ++ "\n"
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

        encodeNumber =
            encodeFloat options.precision

        vertexToRecord { position, normal, uv } =
            { px = Point3d.xCoordinate position
            , py = Point3d.yCoordinate position
            , pz = Point3d.zCoordinate position
            , normal =
                "vn "
                    ++ encodeNumber (Quantity.toFloat (Vector3d.xComponent normal))
                    ++ " "
                    ++ encodeNumber (Quantity.toFloat (Vector3d.yComponent normal))
                    ++ " "
                    ++ encodeNumber (Quantity.toFloat (Vector3d.zComponent normal))
                    ++ "\n"
            , uv =
                "vt "
                    ++ encodeNumber (Tuple.first uv)
                    ++ " "
                    ++ encodeNumber (Tuple.second uv)
                    ++ "\n"
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
    if pts == [] then
        Empty

    else
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


encodePositions : (Length -> String) -> List { a | px : Length, py : Length, pz : Length } -> String -> String
encodePositions lengthToString positions result =
    case positions of
        { px, py, pz } :: remainingPositions ->
            encodePositions lengthToString
                remainingPositions
                (result
                    ++ "v "
                    ++ lengthToString px
                    ++ " "
                    ++ lengthToString py
                    ++ " "
                    ++ lengthToString pz
                    ++ "\n"
                )

        [] ->
            result


encodeNormals : List { a | normal : String } -> String -> String
encodeNormals normals result =
    case normals of
        { normal } :: remainingNormals ->
            encodeNormals remainingNormals (result ++ normal ++ "")

        [] ->
            result


encodeUvs : List { a | uv : String } -> String -> String
encodeUvs uvs result =
    case uvs of
        { uv } :: remainingUvs ->
            encodeUvs remainingUvs (result ++ uv ++ "")

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


encodePolylines : (Length -> String) -> List (List { px : Length, py : Length, pz : Length }) -> List { px : Length, py : Length, pz : Length } -> Int -> String -> String -> ( Int, String )
encodePolylines lengthToString lines positions positionOffset resultVertices resultIndices =
    case positions of
        { px, py, pz } :: remainingPositions ->
            encodePolylines
                lengthToString
                lines
                remainingPositions
                (positionOffset + 1)
                (resultVertices
                    ++ "v "
                    ++ lengthToString px
                    ++ " "
                    ++ lengthToString py
                    ++ " "
                    ++ lengthToString pz
                    ++ "\n"
                )
                (resultIndices ++ " " ++ String.fromInt positionOffset)

        [] ->
            case lines of
                [] ->
                    ( positionOffset, resultVertices ++ resultIndices ++ "\n" )

                [] :: remainingLines ->
                    encodePolylines lengthToString
                        remainingLines
                        []
                        positionOffset
                        resultVertices
                        resultIndices

                nextLine :: remainingLines ->
                    encodePolylines lengthToString
                        remainingLines
                        nextLine
                        positionOffset
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


encodeLength : Int -> (Length -> Float) -> Length -> String
encodeLength precision units =
    let
        encodeNumber =
            encodeFloat precision
    in
    \length -> encodeNumber (units length)


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
                String.fromInt fraction

            incrementedInteger =
                if String.left 1 fractionStr == "2" then
                    integer + 1

                else
                    integer
        in
        if isNaN (float - float) then
            String.padRight (n + 2) '0' "0."

        else if float >= 0 then
            String.fromInt incrementedInteger ++ "." ++ String.dropLeft 1 fractionStr

        else
            "-" ++ String.fromInt incrementedInteger ++ "." ++ String.dropLeft 1 fractionStr
