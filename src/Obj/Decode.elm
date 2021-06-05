module Obj.Decode exposing
    ( Decoder
    , triangles, faces, texturedTriangles, texturedFaces, bumpyFaces, polylines, points
    , decodeString, expectObj
    , object, group, defaultGroup, material
    , objectNames, groupNames, materialNames
    , map, map2, map3, map4, map5
    , filter, oneOf, fail, succeed, andThen, combine
    , ObjCoordinates
    , trianglesIn, facesIn, texturedTrianglesIn, texturedFacesIn, bumpyFacesIn, polylinesIn, pointsIn
    )

{-|

@docs Decoder


# Primitives

elm-obj-file supports triangular meshes that may have normal vectors and/or texture coordinates, polylines and points.

By default, the geometrical data is returned in the `ObjCoordinates` [coordinate system](https://github.com/ianmackenzie/elm-geometry#coordinate-systems).
It's also possible to [transform coordinates](#coordinate-conversion) if desired.

Note that all primitive decoders require at least one element and will fail if no elements are found.

@docs triangles, faces, texturedTriangles, texturedFaces, bumpyFaces, polylines, points


# Run Decoders

@docs decodeString, expectObj


# Filtering

Primitives within OBJ files can be tagged with metadata such as object name, group names and materials.

Using the filtering decoders, you can selectively decode based on this metadata.

For advanced filtering rules check the [`filter`](#filter) decoder.

@docs object, group, defaultGroup, material


# Metadata

Decode useful information other than primitives. This can be useful to inspect the contents of the file.

Metadata decoders can be also composed with advanced decoders [`andThen`](#andThen) and
[`combine`](#combine) to first get the metadata, and then filter the primitives.

@docs objectNames, groupNames, materialNames


# Mapping

@docs map, map2, map3, map4, map5


# Advanced Decoding

@docs filter, oneOf, fail, succeed, andThen, combine


# Coordinate Conversion

@docs ObjCoordinates

@docs trianglesIn, facesIn, texturedTrianglesIn, texturedFacesIn, bumpyFacesIn, polylinesIn, pointsIn

-}

import Array exposing (Array)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Http
import Length exposing (Length, Meters)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import Quantity exposing (Quantity(..), Unitless)
import Set
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


{-| A value that knows how to decode information from
[the OBJ file format](https://en.wikipedia.org/wiki/Wavefront_.obj_file)
-}
type Decoder a
    = Decoder (VertexData -> List String -> List Group -> Result String a)


{-| Decode just the plain positions. Use with `Scene3d.Mesh.indexedTriangles` and `Scene3d.Mesh.indexedFacets` from elm-3d-scene.
-}
triangles : Decoder (TriangularMesh (Point3d Meters ObjCoordinates))
triangles =
    trianglesIn Frame3d.atOrigin


{-| Decode positions and normal vectors. Use with `Scene3d.Mesh.indexedFaces`.
-}
faces : Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, normal : Vector3d Unitless ObjCoordinates })
faces =
    facesIn Frame3d.atOrigin


{-| Decode positions and [UV](https://learnopengl.com/Getting-started/Textures) (texture) coordinates.
Use with `Scene3d.Mesh.texturedTriangles` or `Scene3d.Mesh.texturedFacets`.
-}
texturedTriangles : Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, uv : ( Float, Float ) })
texturedTriangles =
    texturedTrianglesIn Frame3d.atOrigin


{-| Decode positions, UV and normal vectors. Use with `Scene3d.Mesh.texturedFaces`.
-}
texturedFaces : Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, normal : Vector3d Unitless ObjCoordinates, uv : ( Float, Float ) })
texturedFaces =
    texturedFacesIn Frame3d.atOrigin


{-| Decode positions, UV, normal vectors and tangents. Use with `Scene3d.Mesh.bumpyFaces`.
-}
bumpyFaces : Decoder (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ), tangent : Vector3d Unitless coordinates, tangentBasisIsRightHanded : Bool })
bumpyFaces =
    bumpyFacesIn Frame3d.atOrigin


{-| -}
polylines : Decoder (List (Polyline3d Meters ObjCoordinates))
polylines =
    polylinesIn Frame3d.atOrigin


{-| -}
points : Decoder (List (Point3d Meters ObjCoordinates))
points =
    pointsIn Frame3d.atOrigin



-- RUN DECODERS


{-| Run the decoder on the string. Takes a function, that knows
how to convert float coordinates into physical units.

    decodeString Length.meters triangles string == Ok (TriangularMesh {...})
    decodeString Length.meters triangles string == Err "Line 1: Invalid OBJ syntax '...'"

-}
decodeString : (Float -> Length) -> Decoder a -> String -> Result String a
decodeString units (Decoder decode) content =
    let
        unitsFn =
            \n -> Length.inMeters (units n)
    in
    decodeHelp unitsFn decode (String.lines content) 1 [] [] [] [] Nothing Nothing [ "default" ] [] [] []


{-| Load a mesh from an [HTTP request](https://package.elm-lang.org/packages/elm/http/latest/).

    type Msg
        = GotMesh (Result Http.Error (TriangularMesh (Point3d Meters ObjCoordinates)))

    getMesh : Cmd Msg
    getMesh =
        Http.get
            { url = "Pod.obj.txt"
            , expect =
                expectObj GotMesh
                    Length.meters
                    triangles
            }

Note: the .txt extension is required to work with `elm reactor`.

-}
expectObj : (Result Http.Error a -> msg) -> (Float -> Length) -> Decoder a -> Http.Expect msg
expectObj toMsg units decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case decodeString units decoder body of
                        Ok value ->
                            Ok value

                        Err string ->
                            Err (Http.BadBody string)



-- FILTERING


{-| Decode data for the given object name.

    wheels : Decoder (TriangularMesh (Point3d Meters ObjCoordinates))
    wheels =
        object "wheels" triangles

-}
object : String -> Decoder a -> Decoder a
object name =
    filterHelp ("object '" ++ name ++ "'") (\properties -> properties.object == Just name)


{-| Decode data for the given group name.
-}
group : String -> Decoder a -> Decoder a
group name =
    filterHelp ("group '" ++ name ++ "'") (\properties -> List.member name properties.groups)


{-| Decode data for the default group. This group has a special meaning,
all elements are assigned to it if a group is not specified.

    defaultGroup =
        group "default"

-}
defaultGroup : Decoder a -> Decoder a
defaultGroup =
    group "default"


{-| Decode data for the given material name.
-}
material : String -> Decoder a -> Decoder a
material name =
    filterHelp ("material '" ++ name ++ "'") (\properties -> properties.material == Just name)



-- METADATA


{-| Decode a sorted list of object names.
-}
objectNames : Decoder (List String)
objectNames =
    Decoder
        (\_ _ elements ->
            elements
                |> List.foldl
                    (\(Group properties _ _ _) objectsSet ->
                        case properties.object of
                            Just obj ->
                                Set.insert obj objectsSet

                            Nothing ->
                                objectsSet
                    )
                    Set.empty
                |> Set.toList
                |> Result.Ok
        )


{-| Decode a sorted list of group names.
-}
groupNames : Decoder (List String)
groupNames =
    Decoder
        (\_ _ elements ->
            elements
                |> List.foldl
                    (\(Group properties _ _ _) groupsSet ->
                        List.foldl Set.insert groupsSet properties.groups
                    )
                    Set.empty
                |> Set.toList
                |> Result.Ok
        )


{-| Decode a sorted list of material names.
-}
materialNames : Decoder (List String)
materialNames =
    Decoder
        (\_ _ elements ->
            elements
                |> List.foldl
                    (\(Group properties _ _ _) materialsSet ->
                        case properties.material of
                            Just obj ->
                                Set.insert obj materialsSet

                            Nothing ->
                                materialsSet
                    )
                    Set.empty
                |> Set.toList
                |> Result.Ok
        )



-- MAPPING


{-| Transform the decoder. For example, if you need to decode triangles’ vertices:

    vertices : Decoder (List (Point3d Meters ObjCoordinates))
    vertices =
        map
            (\triangularMesh ->
                triangularMesh
                    |> TriangularMesh.vertices
                    |> Array.toList
            )
            triangles

-}
map : (a -> b) -> Decoder a -> Decoder b
map fn (Decoder decoder) =
    Decoder
        (\vertexData filters elements ->
            Result.map fn (decoder vertexData filters elements)
        )


{-| Join the result from two decoders. This lets you extract parts of the same OBJ file into separate meshes.

    type alias Car =
        { wheels : TriangularMesh (Point3d Meters ObjCoordinates)
        , base : TriangularMesh (Point3d Meters ObjCoordinates)
        }

    carDecoder : Decoder Car
    carDecoder =
        map2 Car
            (object "wheels" triangles)
            (object "base" triangles)

-}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 fn (Decoder decoderA) (Decoder decoderB) =
    Decoder
        (\vertexData filters elements ->
            Result.map2 fn
                (decoderA vertexData filters elements)
                (decoderB vertexData filters elements)
        )


{-| -}
map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 fn (Decoder decoderA) (Decoder decoderB) (Decoder decoderC) =
    Decoder
        (\vertexData filters elements ->
            Result.map3 fn
                (decoderA vertexData filters elements)
                (decoderB vertexData filters elements)
                (decoderC vertexData filters elements)
        )


{-| -}
map4 : (a -> b -> c -> d -> e) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e
map4 fn (Decoder decoderA) (Decoder decoderB) (Decoder decoderC) (Decoder decoderD) =
    Decoder
        (\vertexData filters elements ->
            Result.map4 fn
                (decoderA vertexData filters elements)
                (decoderB vertexData filters elements)
                (decoderC vertexData filters elements)
                (decoderD vertexData filters elements)
        )


{-| -}
map5 : (a -> b -> c -> d -> e -> f) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f
map5 fn (Decoder decoderA) (Decoder decoderB) (Decoder decoderC) (Decoder decoderD) (Decoder decoderE) =
    Decoder
        (\vertexData filters elements ->
            Result.map5 fn
                (decoderA vertexData filters elements)
                (decoderB vertexData filters elements)
                (decoderC vertexData filters elements)
                (decoderD vertexData filters elements)
                (decoderE vertexData filters elements)
        )



-- ADVANCED DECODING


{-| Filter what should be decoded. For example, to implement the [`group`](#group) decoder from above:

    group name =
        filter
            (\properties ->
                List.member name properties.groups
            )

-}
filter :
    ({ groups : List String, object : Maybe String, material : Maybe String } -> Bool)
    -> Decoder a
    -> Decoder a
filter =
    filterHelp "<custom filter>"


filterHelp :
    String
    -> ({ groups : List String, object : Maybe String, material : Maybe String } -> Bool)
    -> Decoder a
    -> Decoder a
filterHelp name fn (Decoder decoder) =
    Decoder
        (\vertexData filters elements ->
            decoder vertexData
                (name :: filters)
                (List.filter
                    (\(Group properties _ _ _) -> fn properties)
                    elements
                )
        )


{-| Try a bunch of different decoders. You will get the result from the first one that succeeds.
-}
oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    Decoder
        (\vertexData filters elements ->
            oneOfHelp vertexData filters elements decoders []
        )


oneOfHelp : VertexData -> List String -> List Group -> List (Decoder a) -> List String -> Result String a
oneOfHelp vertexData filters elements decoders errors =
    case decoders of
        (Decoder decoder) :: remainingDecoders ->
            case decoder vertexData filters elements of
                Ok res ->
                    Ok res

                Err error ->
                    oneOfHelp vertexData filters elements remainingDecoders (error :: errors)

        [] ->
            case errors of
                _ :: _ ->
                    Err ("Failed oneOf decoder: " ++ String.join ", " (List.reverse errors) ++ ".")

                [] ->
                    Err "Empty oneOf decoder"


{-| A decoder that always succeeds with the result. May be useful in combination with [`oneOf`](#oneOf) to
provide a placeholder mesh if decoding fails.
-}
succeed : a -> Decoder a
succeed mesh =
    Decoder (\_ _ _ -> Result.Ok mesh)


{-| A decoder that always fails with a given error message.
Use it in case you need custom error messages.
-}
fail : String -> Decoder a
fail error =
    Decoder (\_ _ _ -> Result.Err error)


{-| Run one decoder and then run another decoder. Useful when you first want to look at metadata,
and then filter based on that.
-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen fn (Decoder decoderA) =
    Decoder
        (\vertexData filters elements ->
            case decoderA vertexData filters elements of
                Ok result ->
                    case fn result of
                        Decoder decoderB ->
                            decoderB vertexData [] elements

                Err error ->
                    Err error
        )


{-| Combine multiple decoders together. For example, to extract meshes for all materials:

    type alias MeshWithMaterial =
        ( String, TriangularMesh (Point3d Meters ObjCoordinates) )

    trianglesForMaterials : String -> Decode (List MeshWithMaterial)
    trianglesForMaterials names =
        names
            |> List.map
                (\materialName ->
                    material materialName triangles
                        |> map (\mesh -> ( materialName, mesh ))
                )
            |> combine

    -- Decode material names, and then decode
    -- triangles for these materials
    withMaterials : Decode (List MeshWithMaterial)
    withMaterials =
        materialNames |> andThen trianglesForMaterials

-}
combine : List (Decoder a) -> Decoder (List a)
combine decoders =
    Decoder
        (\vertexData filters elements ->
            combineHelp vertexData filters elements decoders []
        )


combineHelp : VertexData -> List String -> List Group -> List (Decoder a) -> List a -> Result String (List a)
combineHelp vertexData filters elements decoders list =
    case decoders of
        (Decoder decoder) :: remainingDecoders ->
            case decoder vertexData filters elements of
                Ok result ->
                    combineHelp vertexData filters elements remainingDecoders (result :: list)

                Err error ->
                    Err error

        [] ->
            Ok (List.reverse list)


{-| Coordinate system for decoded meshes.
-}
type ObjCoordinates
    = ObjCoordinates


{-| Transform coordinates when decoding. For example, if you need to render a mesh with Z-up,
but it was exported with Y-up:

    type ZUpCoords
        = ZUpCoords

    yUpToZUpFrame : Frame3d Meters ZUpCoords { defines : ObjCoordinates }
    yUpToZUpFrame =
        Frame3d.atOrigin
            |> Frame3d.rotateAround
                Axis3d.x
                (Angle.degrees 90)

    zUpTriangles : Decoder (TriangularMesh (Point3d Meters ZUpCoords))
    zUpTriangles =
        trianglesIn yUpToZUpFrame

-}
trianglesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh (Point3d Meters coordinates))
trianglesIn frame =
    Decoder
        (\vertexData filters groups ->
            triangularMesh
                (addTriangles vertexData frame)
                filters
                groups
                (indexState vertexData.indexMap)
                []
        )


{-| -}
facesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates })
facesIn frame =
    Decoder
        (\vertexData filters groups ->
            triangularMesh
                (addFaces vertexData frame)
                filters
                groups
                (indexState vertexData.indexMap)
                []
        )


{-| -}
texturedTrianglesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh { position : Point3d Meters coordinates, uv : ( Float, Float ) })
texturedTrianglesIn frame =
    Decoder
        (\vertexData filters groups ->
            triangularMesh
                (addTexturedTriangles vertexData frame)
                filters
                groups
                (indexState vertexData.indexMap)
                []
        )


{-| -}
texturedFacesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ) })
texturedFacesIn frame =
    Decoder
        (\vertexData filters groups ->
            triangularMesh
                (addTexturedFaces vertexData frame)
                filters
                groups
                (indexState vertexData.indexMap)
                []
        )


{-| -}
bumpyFacesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ), tangent : Vector3d Unitless coordinates, tangentBasisIsRightHanded : Bool })
bumpyFacesIn frame =
    Decoder
        (\vertexData filters groups ->
            triangularMesh
                (addbumpyFaces vertexData frame)
                filters
                groups
                (indexState vertexData.indexMap)
                []
                |> Result.map computeTangents
        )


computeTangents : TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ), tangent : Vector3d Unitless coordinates, bitangent : Vector3d Unitless coordinates } -> TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ), tangent : Vector3d Unitless coordinates, tangentBasisIsRightHanded : Bool }
computeTangents mesh =
    let
        faceIndices =
            TriangularMesh.faceIndices mesh

        newVertices =
            List.foldl
                (\( i1, i2, i3 ) vertices ->
                    case Array.get i1 vertices of
                        Just vertex1 ->
                            case Array.get i2 vertices of
                                Just vertex2 ->
                                    case Array.get i3 vertices of
                                        Just vertex3 ->
                                            let
                                                p1 =
                                                    Point3d.toMeters vertex1.position

                                                p2 =
                                                    Point3d.toMeters vertex2.position

                                                p3 =
                                                    Point3d.toMeters vertex3.position

                                                ( u1, v1 ) =
                                                    vertex1.uv

                                                ( u2, v2 ) =
                                                    vertex2.uv

                                                ( u3, v3 ) =
                                                    vertex3.uv

                                                dX1 =
                                                    p2.x - p1.x

                                                dX2 =
                                                    p3.x - p1.x

                                                dY1 =
                                                    p2.y - p1.y

                                                dY2 =
                                                    p3.y - p1.y

                                                dZ1 =
                                                    p2.z - p1.z

                                                dZ2 =
                                                    p3.z - p1.z

                                                dU1 =
                                                    u2 - u1

                                                dU2 =
                                                    u3 - u1

                                                dV1 =
                                                    v2 - v1

                                                dV2 =
                                                    v3 - v1

                                                r =
                                                    1.0 / (dU1 * dV2 - dV1 * dU2)

                                                tangent =
                                                    Vector3d.unitless
                                                        ((dX1 * dV2 - dX2 * dV1) * r)
                                                        ((dY1 * dV2 - dY2 * dV1) * r)
                                                        ((dZ1 * dV2 - dZ2 * dV1) * r)

                                                bitangent =
                                                    Vector3d.unitless
                                                        ((dX2 * dU1 - dX1 * dU2) * r)
                                                        ((dY2 * dU1 - dY1 * dU2) * r)
                                                        ((dZ2 * dU1 - dZ1 * dU2) * r)
                                            in
                                            vertices
                                                |> Array.set i1
                                                    { normal = vertex1.normal
                                                    , position = vertex1.position
                                                    , uv = vertex1.uv
                                                    , tangent = Vector3d.plus tangent vertex1.tangent
                                                    , bitangent = Vector3d.plus bitangent vertex1.bitangent
                                                    }
                                                |> Array.set i2
                                                    { normal = vertex2.normal
                                                    , position = vertex2.position
                                                    , uv = vertex2.uv
                                                    , tangent = Vector3d.plus tangent vertex2.tangent
                                                    , bitangent = Vector3d.plus bitangent vertex2.bitangent
                                                    }
                                                |> Array.set i3
                                                    { normal = vertex3.normal
                                                    , position = vertex3.position
                                                    , uv = vertex3.uv
                                                    , tangent = Vector3d.plus tangent vertex3.tangent
                                                    , bitangent = Vector3d.plus bitangent vertex3.bitangent
                                                    }

                                        Nothing ->
                                            vertices

                                Nothing ->
                                    vertices

                        Nothing ->
                            vertices
                )
                (TriangularMesh.vertices mesh)
                faceIndices
    in
    TriangularMesh.mapVertices
        (\v ->
            let
                (Quantity dot) =
                    Vector3d.dot v.tangent v.normal

                tangent =
                    Vector3d.normalize (Vector3d.minus (Vector3d.scaleBy dot v.normal) v.tangent)

                (Quantity handednessDot) =
                    Vector3d.dot (Vector3d.cross v.normal v.tangent) v.bitangent
            in
            { position = v.position
            , uv = v.uv
            , tangent = tangent
            , normal = v.normal
            , tangentBasisIsRightHanded = handednessDot > 0
            }
        )
        (TriangularMesh.indexed newVertices faceIndices)


{-| -}
polylinesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (List (Polyline3d Meters coordinates))
polylinesIn frame =
    Decoder
        (\{ positions } filters groups ->
            addPolylines (Settings positions frame filters)
                groups
                []
                0
                []
                []
                []
        )


{-| -}
pointsIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (List (Point3d Meters coordinates))
pointsIn frame =
    Decoder
        (\{ positions } filters groups ->
            addPoints (Settings positions frame filters)
                groups
                []
                0
                []
                []
        )



-- Internals


type alias Face coordinates =
    { position : Point3d Meters coordinates
    , normal : Vector3d Unitless coordinates
    }


type alias TexturedTriangle coordinates =
    { position : Point3d Meters coordinates
    , uv : ( Float, Float )
    }


type alias TexturedFace coordinates =
    { position : Point3d Meters coordinates
    , normal : Vector3d Unitless coordinates
    , uv : ( Float, Float )
    }


type alias VertexData =
    { positions : Array (Point3d Meters ObjCoordinates)
    , normals : Array (Direction3d ObjCoordinates)
    , uvs : Array ( Float, Float )
    , indexMap : Array (List Int)
    }


type FaceElement
    = FaceElement Int (List Vertex)


type LineElement
    = LineElement Int (List Vertex)


type PointsElement
    = PointsElement Int (List Vertex)


{-| Stores indices into positions, uv coordinates and normal.
Position index is always there. We use -1 for the missing uv or normal index.
-}
type alias Vertex =
    { p : Int, uv : Int, n : Int }


type Group
    = Group
        { groups : List String
        , object : Maybe String
        , material : Maybe String
        }
        (List FaceElement)
        (List LineElement)
        (List PointsElement)


type alias IndexState a =
    { maxIndex : Int
    , indexMap : Array (List Int)
    , vertices : List a
    }


indexState : Array (List Int) -> IndexState a
indexState indexMap =
    { maxIndex = -1
    , indexMap = indexMap
    , vertices = []
    }


{-| Defines a function that knows how to collect a certain kind of triangle
-}
type alias AddIndexedTriangles a =
    Int
    -> List Vertex
    -> List FaceElement
    -> Int
    -> Array (List Int)
    -> List a
    -> List Int
    -> List ( Int, Int, Int )
    -> Result String ( IndexState a, List ( Int, Int, Int ) )


triangularMesh : AddIndexedTriangles a -> List String -> List Group -> IndexState a -> List ( Int, Int, Int ) -> Result String (TriangularMesh a)
triangularMesh add filters groups ({ maxIndex, indexMap, vertices } as currentIndexedState) faceIndices =
    case groups of
        (Group _ ((FaceElement lineno elementVertices) :: faceElements) _ _) :: remainingElementGroups ->
            case add lineno elementVertices faceElements maxIndex indexMap vertices [] faceIndices of
                Ok ( newIndexedState, newFaceIndices ) ->
                    triangularMesh add filters remainingElementGroups newIndexedState newFaceIndices

                Err error ->
                    Err error

        (Group _ [] _ _) :: remainingElementGroups ->
            -- skip an empty group
            triangularMesh add filters remainingElementGroups currentIndexedState faceIndices

        [] ->
            case faceIndices of
                _ :: _ ->
                    Ok (TriangularMesh.indexed (Array.fromList (List.reverse vertices)) faceIndices)

                [] ->
                    case filters of
                        _ :: _ ->
                            Err ("No faces found for " ++ String.join ", " filters)

                        [] ->
                            Err "No faces found"


groupIndices : Int -> List Int -> List ( Int, Int, Int ) -> List ( Int, Int, Int )
groupIndices p1 more result =
    case more of
        p2 :: rest ->
            case rest of
                p3 :: _ ->
                    -- Note that when it comes to grouping, the order of points is reversed
                    -- but the indices were reversed too, when parsing, so this is fine :-)
                    groupIndices p1 rest (( p1, p2, p3 ) :: result)

                [] ->
                    result

        [] ->
            result


addTriangles : VertexData -> Frame3d Meters coordinates { defines : ObjCoordinates } -> AddIndexedTriangles (Point3d Meters coordinates)
addTriangles vertexData frame lineno elementVertices elements maxIndex indexMap vertices indices faceIndices =
    case elementVertices of
        { p } :: remainingVertices ->
            case Array.get p indexMap of
                Just [ idx ] ->
                    addTriangles vertexData
                        frame
                        lineno
                        remainingVertices
                        elements
                        maxIndex
                        indexMap
                        vertices
                        (idx :: indices)
                        faceIndices

                _ ->
                    case Array.get p vertexData.positions of
                        Just vertex ->
                            addTriangles vertexData
                                frame
                                lineno
                                remainingVertices
                                elements
                                (maxIndex + 1)
                                (Array.set p [ maxIndex + 1 ] indexMap)
                                (Point3d.placeIn frame vertex :: vertices)
                                (maxIndex + 1 :: indices)
                                faceIndices

                        Nothing ->
                            formatError lineno "Index out of range"

        [] ->
            let
                newFaceIndices =
                    case indices of
                        p1 :: remainingIndices ->
                            -- parser guarantees at least 3 face indices
                            groupIndices p1 remainingIndices faceIndices

                        [] ->
                            faceIndices
            in
            case elements of
                (FaceElement newLineno newElementVertices) :: remainingElements ->
                    addTriangles vertexData
                        frame
                        newLineno
                        newElementVertices
                        remainingElements
                        maxIndex
                        indexMap
                        vertices
                        []
                        newFaceIndices

                [] ->
                    Ok ( { maxIndex = maxIndex, indexMap = indexMap, vertices = vertices }, newFaceIndices )


addFaces : VertexData -> Frame3d Meters coordinates { defines : ObjCoordinates } -> AddIndexedTriangles (Face coordinates)
addFaces vertexData frame lineno elementVertices elements maxIndex indexMap vertices indices faceIndices =
    case elementVertices of
        { p, n } :: remainingVertices ->
            if n > -1 then
                let
                    lookupArray =
                        Maybe.withDefault [] (Array.get p indexMap)

                    idx =
                        lookup1 n lookupArray
                in
                if idx > -1 then
                    addFaces vertexData
                        frame
                        lineno
                        remainingVertices
                        elements
                        maxIndex
                        indexMap
                        vertices
                        (idx :: indices)
                        faceIndices

                else
                    -- pattern match for performance
                    case Array.get p vertexData.positions of
                        Just position ->
                            case Array.get n vertexData.normals of
                                Just normal ->
                                    addFaces vertexData
                                        frame
                                        lineno
                                        remainingVertices
                                        elements
                                        (maxIndex + 1)
                                        (Array.set p (n :: maxIndex + 1 :: lookupArray) indexMap)
                                        ({ position = Point3d.placeIn frame position
                                         , normal = Direction3d.toVector (Direction3d.placeIn frame normal)
                                         }
                                            :: vertices
                                        )
                                        (maxIndex + 1 :: indices)
                                        faceIndices

                                Nothing ->
                                    formatError lineno "Index out of range"

                        Nothing ->
                            formatError lineno "Index out of range"

            else
                formatError lineno "Vertex has no normal vector"

        [] ->
            let
                newFaceIndices =
                    case indices of
                        p1 :: remainingIndices ->
                            -- parser guarantees at least 3 face indices
                            groupIndices p1 remainingIndices faceIndices

                        [] ->
                            faceIndices
            in
            case elements of
                (FaceElement newLineno newElementVertices) :: remainingElements ->
                    addFaces vertexData
                        frame
                        newLineno
                        newElementVertices
                        remainingElements
                        maxIndex
                        indexMap
                        vertices
                        []
                        newFaceIndices

                [] ->
                    Ok ( { maxIndex = maxIndex, indexMap = indexMap, vertices = vertices }, newFaceIndices )


addTexturedTriangles : VertexData -> Frame3d Meters coordinates { defines : ObjCoordinates } -> AddIndexedTriangles (TexturedTriangle coordinates)
addTexturedTriangles vertexData frame lineno elementVertices elements maxIndex indexMap vertices indices faceIndices =
    case elementVertices of
        { p, uv } :: remainingVertices ->
            if uv > -1 then
                let
                    lookupArray =
                        Maybe.withDefault [] (Array.get p indexMap)

                    idx =
                        lookup1 uv lookupArray
                in
                if idx > -1 then
                    addTexturedTriangles vertexData
                        frame
                        lineno
                        remainingVertices
                        elements
                        maxIndex
                        indexMap
                        vertices
                        (idx :: indices)
                        faceIndices

                else
                    -- pattern match for performance
                    case Array.get p vertexData.positions of
                        Just position ->
                            case Array.get uv vertexData.uvs of
                                Just uvCoord ->
                                    addTexturedTriangles vertexData
                                        frame
                                        lineno
                                        remainingVertices
                                        elements
                                        (maxIndex + 1)
                                        (Array.set p (uv :: maxIndex + 1 :: lookupArray) indexMap)
                                        ({ position = Point3d.placeIn frame position
                                         , uv = uvCoord
                                         }
                                            :: vertices
                                        )
                                        (maxIndex + 1 :: indices)
                                        faceIndices

                                Nothing ->
                                    formatError lineno "Index out of range"

                        Nothing ->
                            formatError lineno "Index out of range"

            else
                formatError lineno "Vertex has no texture coordinates"

        [] ->
            let
                newFaceIndices =
                    case indices of
                        p1 :: remainingIndices ->
                            -- parser guarantees at least 3 face indices
                            groupIndices p1 remainingIndices faceIndices

                        [] ->
                            faceIndices
            in
            case elements of
                (FaceElement newLineno newElementVertices) :: remainingElements ->
                    addTexturedTriangles vertexData
                        frame
                        newLineno
                        newElementVertices
                        remainingElements
                        maxIndex
                        indexMap
                        vertices
                        []
                        newFaceIndices

                [] ->
                    Ok ( { maxIndex = maxIndex, indexMap = indexMap, vertices = vertices }, newFaceIndices )


addTexturedFaces : VertexData -> Frame3d Meters coordinates { defines : ObjCoordinates } -> AddIndexedTriangles (TexturedFace coordinates)
addTexturedFaces vertexData frame lineno elementVertices elements maxIndex indexMap vertices indices faceIndices =
    case elementVertices of
        { p, uv, n } :: remainingVertices ->
            if uv > -1 && n > -1 then
                let
                    lookupArray =
                        Maybe.withDefault [] (Array.get p indexMap)

                    idx =
                        lookup2 uv n lookupArray
                in
                if idx > -1 then
                    addTexturedFaces vertexData
                        frame
                        lineno
                        remainingVertices
                        elements
                        maxIndex
                        indexMap
                        vertices
                        (idx :: indices)
                        faceIndices

                else
                    -- pattern match for performance
                    case Array.get p vertexData.positions of
                        Just position ->
                            case Array.get n vertexData.normals of
                                Just normal ->
                                    case Array.get uv vertexData.uvs of
                                        Just uvCoord ->
                                            addTexturedFaces vertexData
                                                frame
                                                lineno
                                                remainingVertices
                                                elements
                                                (maxIndex + 1)
                                                (Array.set p (uv :: n :: maxIndex + 1 :: lookupArray) indexMap)
                                                ({ position = Point3d.placeIn frame position
                                                 , normal = Direction3d.toVector (Direction3d.placeIn frame normal)
                                                 , uv = uvCoord
                                                 }
                                                    :: vertices
                                                )
                                                (maxIndex + 1 :: indices)
                                                faceIndices

                                        Nothing ->
                                            formatError lineno "Index out of range"

                                Nothing ->
                                    formatError lineno "Index out of range"

                        Nothing ->
                            formatError lineno "Index out of range"

            else
                formatError lineno "Vertex missing normal vector and/or texture coordinates"

        [] ->
            let
                newFaceIndices =
                    case indices of
                        p1 :: remainingIndices ->
                            -- parser guarantees at least 3 face indices
                            groupIndices p1 remainingIndices faceIndices

                        [] ->
                            faceIndices
            in
            case elements of
                (FaceElement newLineno newElementVertices) :: remainingElements ->
                    addTexturedFaces vertexData
                        frame
                        newLineno
                        newElementVertices
                        remainingElements
                        maxIndex
                        indexMap
                        vertices
                        []
                        newFaceIndices

                [] ->
                    Ok ( { maxIndex = maxIndex, indexMap = indexMap, vertices = vertices }, newFaceIndices )


addbumpyFaces :
    VertexData
    -> Frame3d Meters coordinates { defines : ObjCoordinates }
    ->
        AddIndexedTriangles
            { position : Point3d Meters coordinates
            , normal : Vector3d Unitless coordinates
            , uv : ( Float, Float )
            , tangent : Vector3d Unitless coordinates
            , bitangent : Vector3d Unitless coordinates
            }
addbumpyFaces vertexData frame lineno elementVertices elements maxIndex indexMap vertices indices faceIndices =
    case elementVertices of
        { p, uv, n } :: remainingVertices ->
            if uv > -1 && n > -1 then
                let
                    lookupArray =
                        Maybe.withDefault [] (Array.get p indexMap)

                    idx =
                        lookup2 uv n lookupArray
                in
                if idx > -1 then
                    addbumpyFaces vertexData
                        frame
                        lineno
                        remainingVertices
                        elements
                        maxIndex
                        indexMap
                        vertices
                        (idx :: indices)
                        faceIndices

                else
                    -- pattern match for performance
                    case Array.get p vertexData.positions of
                        Just position ->
                            case Array.get n vertexData.normals of
                                Just normal ->
                                    case Array.get uv vertexData.uvs of
                                        Just uvCoord ->
                                            addbumpyFaces vertexData
                                                frame
                                                lineno
                                                remainingVertices
                                                elements
                                                (maxIndex + 1)
                                                (Array.set p (uv :: n :: maxIndex + 1 :: lookupArray) indexMap)
                                                ({ position = Point3d.placeIn frame position
                                                 , normal = Direction3d.toVector (Direction3d.placeIn frame normal)
                                                 , tangent = Vector3d.unitless 0 0 0
                                                 , bitangent = Vector3d.unitless 0 0 0
                                                 , uv = uvCoord
                                                 }
                                                    :: vertices
                                                )
                                                (maxIndex + 1 :: indices)
                                                faceIndices

                                        Nothing ->
                                            formatError lineno "Index out of range"

                                Nothing ->
                                    formatError lineno "Index out of range"

                        Nothing ->
                            formatError lineno "Index out of range"

            else
                formatError lineno "Vertex missing normal vector and/or texture coordinates"

        [] ->
            let
                newFaceIndices =
                    case indices of
                        p1 :: remainingIndices ->
                            -- parser guarantees at least 3 face indices
                            groupIndices p1 remainingIndices faceIndices

                        [] ->
                            faceIndices
            in
            case elements of
                (FaceElement newLineno newElementVertices) :: remainingElements ->
                    addbumpyFaces vertexData
                        frame
                        newLineno
                        newElementVertices
                        remainingElements
                        maxIndex
                        indexMap
                        vertices
                        []
                        newFaceIndices

                [] ->
                    Ok ( { maxIndex = maxIndex, indexMap = indexMap, vertices = vertices }, newFaceIndices )


{-| returns -1 if not found
-}
lookup2 : Int -> Int -> List Int -> Int
lookup2 idx1 idx2 list =
    case list of
        i1 :: i2 :: result :: rest ->
            if idx1 - i1 == 0 && idx2 - i2 == 0 then
                result

            else
                lookup2 idx1 idx2 rest

        _ ->
            -1


{-| returns -1 if not found
-}
lookup1 : Int -> List Int -> Int
lookup1 idx1 list =
    case list of
        i1 :: result :: rest ->
            if idx1 - i1 == 0 then
                result

            else
                lookup1 idx1 rest

        _ ->
            -1


type alias Settings coordinates =
    { positions : Array (Point3d Meters ObjCoordinates)
    , frame : Frame3d Meters coordinates { defines : ObjCoordinates }
    , filters : List String
    }


addPolylines :
    Settings coordinates
    -> List Group
    -> List LineElement
    -> Int
    -> List Vertex
    -> List (Point3d Meters coordinates)
    -> List (Polyline3d Meters coordinates)
    -> Result String (List (Polyline3d Meters coordinates))
addPolylines settings groups elements lineno vertices points_ result =
    case vertices of
        { p } :: remainingVertices ->
            case Array.get p settings.positions of
                Just point ->
                    addPolylines settings
                        groups
                        elements
                        lineno
                        remainingVertices
                        (Point3d.placeIn settings.frame point :: points_)
                        result

                Nothing ->
                    formatError lineno "Index out of range"

        [] ->
            let
                newResult =
                    case points_ of
                        _ :: _ ->
                            -- the points are reversed, but the original indices
                            -- were reversed too in the parser
                            Polyline3d.fromVertices points_ :: result

                        [] ->
                            result
            in
            case elements of
                (LineElement newLineno newVertices) :: remainingElements ->
                    addPolylines settings
                        groups
                        remainingElements
                        newLineno
                        newVertices
                        []
                        newResult

                [] ->
                    case groups of
                        (Group _ _ newElements _) :: remainingGroups ->
                            addPolylines settings
                                remainingGroups
                                newElements
                                0
                                []
                                []
                                newResult

                        [] ->
                            case newResult of
                                _ :: _ ->
                                    Ok newResult

                                [] ->
                                    case settings.filters of
                                        _ :: _ ->
                                            Err ("No lines found for " ++ String.join ", " settings.filters)

                                        [] ->
                                            Err "No lines found"


addPoints :
    Settings coordinates
    -> List Group
    -> List PointsElement
    -> Int
    -> List Vertex
    -> List (Point3d Meters coordinates)
    -> Result String (List (Point3d Meters coordinates))
addPoints settings groups elements lineno vertices result =
    case vertices of
        { p } :: remainingVertices ->
            case Array.get p settings.positions of
                Just point ->
                    addPoints settings
                        groups
                        elements
                        lineno
                        remainingVertices
                        (Point3d.placeIn settings.frame point :: result)

                Nothing ->
                    formatError lineno "Index out of range"

        [] ->
            case elements of
                (PointsElement newLineno newVertices) :: remainingElements ->
                    addPoints settings
                        groups
                        remainingElements
                        newLineno
                        newVertices
                        result

                [] ->
                    case groups of
                        (Group _ _ _ newElements) :: remainingGroups ->
                            addPoints settings
                                remainingGroups
                                newElements
                                0
                                []
                                result

                        [] ->
                            case result of
                                _ :: _ ->
                                    Ok result

                                [] ->
                                    case settings.filters of
                                        _ :: _ ->
                                            Err ("No points found for " ++ String.join ", " settings.filters)

                                        [] ->
                                            Err "No points found"


decodeHelp :
    (Float -> Float)
    -> (VertexData -> List String -> List Group -> Result String a)
    -> List String
    -> Int
    -> List (Point3d Meters ObjCoordinates)
    -> List (Direction3d ObjCoordinates)
    -> List ( Float, Float )
    -> List Group
    -> Maybe String
    -> Maybe String
    -> List String
    -> List FaceElement
    -> List LineElement
    -> List PointsElement
    -> Result String a
decodeHelp units decode lines lineno positions normals uvs groups object_ material_ groups_ faceElements lineElements pointsElements =
    case lines of
        line :: remainingLines ->
            let
                words =
                    String.words line

                startsWith =
                    case words of
                        firstWord :: _ ->
                            firstWord

                        _ ->
                            ""

                remainingWords =
                    List.drop 1 words
            in
            -- conditions are sorted based on their frequency
            if startsWith == "v" then
                let
                    ({ x } as position) =
                        parsePosition units remainingWords
                in
                if isNaN x then
                    formatError lineno "Invalid position format"

                else
                    decodeHelp units decode remainingLines (lineno + 1) (Point3d.fromMeters position :: positions) normals uvs groups object_ material_ groups_ faceElements lineElements pointsElements

            else if startsWith == "vt" then
                let
                    (( u, _ ) as uv) =
                        parseUv remainingWords
                in
                if isNaN u then
                    formatError lineno "Invalid texture coordinates format"

                else
                    decodeHelp units decode remainingLines (lineno + 1) positions normals (uv :: uvs) groups object_ material_ groups_ faceElements lineElements pointsElements

            else if startsWith == "vn" then
                -- there is no other keyword that starts with "vn", so we assume the space after "vn "
                let
                    ({ x } as normal) =
                        parseNormal remainingWords
                in
                if isNaN x then
                    formatError lineno "Invalid normal vector format"

                else
                    decodeHelp units decode remainingLines (lineno + 1) positions (Direction3d.unsafe normal :: normals) uvs groups object_ material_ groups_ faceElements lineElements pointsElements

            else if startsWith == "f" then
                case parseIndices remainingWords [] of
                    (_ :: _ :: _ :: _) as vertices ->
                        decodeHelp units decode remainingLines (lineno + 1) positions normals uvs groups object_ material_ groups_ (FaceElement lineno vertices :: faceElements) lineElements pointsElements

                    _ :: _ ->
                        formatError lineno "Face has less than three vertices"

                    [] ->
                        case remainingWords of
                            [] ->
                                formatError lineno "Face has less than three vertices"

                            _ ->
                                formatError lineno "Invalid face format"

            else if startsWith == "l" then
                case parseIndices remainingWords [] of
                    (_ :: _ :: _) as vertices ->
                        decodeHelp units decode remainingLines (lineno + 1) positions normals uvs groups object_ material_ groups_ faceElements (LineElement lineno vertices :: lineElements) pointsElements

                    _ :: _ ->
                        formatError lineno "Line has less than two vertices"

                    [] ->
                        case remainingWords of
                            [] ->
                                formatError lineno "Line has less than two vertices"

                            _ ->
                                formatError lineno "Invalid line format"

            else if startsWith == "p" then
                case parseIndices remainingWords [] of
                    (_ :: _) as vertices ->
                        decodeHelp units decode remainingLines (lineno + 1) positions normals uvs groups object_ material_ groups_ faceElements lineElements (PointsElement lineno vertices :: pointsElements)

                    [] ->
                        case remainingWords of
                            [] ->
                                formatError lineno "Points element has no vertices"

                            _ ->
                                formatError lineno "Invalid points format"

            else if startsWith == "o" then
                case remainingWords of
                    newObject :: _ ->
                        decodeHelp units decode remainingLines (lineno + 1) positions normals uvs (addNonEmptyGroup object_ material_ groups_ faceElements lineElements pointsElements groups) (Just newObject) material_ groups_ [] [] []

                    [] ->
                        formatError lineno "No object name"

            else if startsWith == "g" then
                case remainingWords of
                    [] ->
                        decodeHelp units decode remainingLines (lineno + 1) positions normals uvs (addNonEmptyGroup object_ material_ groups_ faceElements lineElements pointsElements groups) object_ material_ [ "default" ] [] [] []

                    newGroups ->
                        decodeHelp units decode remainingLines (lineno + 1) positions normals uvs (addNonEmptyGroup object_ material_ groups_ faceElements lineElements pointsElements groups) object_ material_ newGroups [] [] []

            else if startsWith == "usemtl" then
                case remainingWords of
                    newMaterial :: _ ->
                        decodeHelp units decode remainingLines (lineno + 1) positions normals uvs (addNonEmptyGroup object_ material_ groups_ faceElements lineElements pointsElements groups) object_ (Just newMaterial) groups_ [] [] []

                    [] ->
                        formatError lineno "No material name"

            else if startsWith == "" || String.left 1 startsWith == "#" || List.any ((==) startsWith) skipCommands then
                -- Skip empty lines, comments and unsupported commands
                decodeHelp units decode remainingLines (lineno + 1) positions normals uvs groups object_ material_ groups_ faceElements lineElements pointsElements

            else
                formatError lineno
                    ("Invalid OBJ syntax '"
                        ++ (if String.length line > 20 then
                                String.left 20 line ++ "...'"

                            else
                                line ++ "'"
                           )
                    )

        [] ->
            let
                positions_ =
                    Array.fromList (List.reverse positions)
            in
            decode
                { positions = positions_
                , normals = Array.fromList (List.reverse normals)
                , uvs = Array.fromList (List.reverse uvs)
                , indexMap = Array.repeat (Array.length positions_) []
                }
                []
                -- flush the last group
                (addNonEmptyGroup object_ material_ groups_ faceElements lineElements pointsElements groups)


addNonEmptyGroup : Maybe String -> Maybe String -> List String -> List FaceElement -> List LineElement -> List PointsElement -> List Group -> List Group
addNonEmptyGroup object_ material_ groups_ faceElements lineElements pointsElements groups =
    case faceElements of
        _ :: _ ->
            Group { groups = groups_, object = object_, material = material_ } faceElements lineElements pointsElements :: groups

        [] ->
            case lineElements of
                _ :: _ ->
                    Group { groups = groups_, object = object_, material = material_ } faceElements lineElements pointsElements :: groups

                [] ->
                    case pointsElements of
                        _ :: _ ->
                            Group { groups = groups_, object = object_, material = material_ } faceElements lineElements pointsElements :: groups

                        [] ->
                            groups


skipCommands : List String
skipCommands =
    [ -- Grouping
      "s" -- smoothing group
    , "mg" -- merging group

    -- Display/render attributes
    , "mtllib" -- material library
    , "bevel" -- bevel interpolation
    , "c_interp" -- color interpolation
    , "d_interp" -- dissolve interpolation
    , "lod" -- level of detail
    , "shadow_obj" -- shadow casting
    , "trace_obj" -- ray tracing
    , "ctech" -- curve approximation technique
    , "stech" -- surface approximation technique

    -- Free-form curve/surface attributes
    , "cstype" -- forms of curve or surface type
    , "deg" -- degree
    , "bmat" -- basis matrix
    , "step" -- step size

    -- Elements
    , "curv" -- curve
    , "curv2" -- 2D curve
    , "surf" -- surface

    -- Free-form curve/surface body statements
    , "parm" -- parameter values
    , "trim" -- outer trimming loop
    , "hole" -- inner trimming loop
    , "scrv" -- special curve
    , "sp" -- special point
    , "end" -- end statement

    -- Connectivity between free-form surfaces
    , "con" -- connect

    -- General statement
    , "call"
    , "scmp"
    , "csh"
    ]


nanXyz : { x : Float, y : Float, z : Float }
nanXyz =
    { x = 0 / 0, y = 0 / 0, z = 0 / 0 }


nanUv : ( Float, Float )
nanUv =
    ( 0 / 0, 0 / 0 )


parsePosition : (Float -> Float) -> List String -> { x : Float, y : Float, z : Float }
parsePosition units list =
    -- sometimes position has more than 3 components, with the 4th component
    -- being the optional weight, that is only required for rational curves and surfaces
    -- we ignore everything after x y z for performance
    case list of
        sx :: sy :: sz :: _ ->
            case String.toFloat sx of
                Just x ->
                    case String.toFloat sy of
                        Just y ->
                            case String.toFloat sz of
                                Just z ->
                                    { x = units x
                                    , y = units y
                                    , z = units z
                                    }

                                Nothing ->
                                    nanXyz

                        Nothing ->
                            nanXyz

                Nothing ->
                    nanXyz

        _ ->
            nanXyz


parseUv : List String -> ( Float, Float )
parseUv list =
    -- sometimes uv has more than 2 components, with the 3rd component
    -- being the optional depth of the texture
    -- we ignore everything after u v for performance
    case list of
        su :: sv :: _ ->
            case String.toFloat su of
                Just u ->
                    case String.toFloat sv of
                        Just v ->
                            ( u, v )

                        Nothing ->
                            nanUv

                Nothing ->
                    nanUv

        su :: [] ->
            -- set the default v=0 if it is missing
            case String.toFloat su of
                Just u ->
                    ( u, 0 )

                Nothing ->
                    nanUv

        _ ->
            nanUv


parseNormal : List String -> { x : Float, y : Float, z : Float }
parseNormal list =
    -- we ignore everything after x y z for performance
    case list of
        sx :: sy :: sz :: _ ->
            case String.toFloat sx of
                Just x ->
                    case String.toFloat sy of
                        Just y ->
                            case String.toFloat sz of
                                Just z ->
                                    { x = x, y = y, z = z }

                                Nothing ->
                                    nanXyz

                        Nothing ->
                            nanXyz

                Nothing ->
                    nanXyz

        _ ->
            nanXyz


parseIndices : List String -> List Vertex -> List Vertex
parseIndices list vertices =
    case list of
        first :: more ->
            case String.split "/" first of
                pComponent :: uvnComponents ->
                    case String.toInt pComponent of
                        Just p ->
                            case uvnComponents of
                                uvComponent :: nComponents ->
                                    case String.toInt uvComponent of
                                        Just uv ->
                                            case nComponents of
                                                nComponent :: _ ->
                                                    case String.toInt nComponent of
                                                        Just n ->
                                                            parseIndices more
                                                                ({ p = p - 1, uv = uv - 1, n = n - 1 } :: vertices)

                                                        Nothing ->
                                                            []

                                                [] ->
                                                    parseIndices more
                                                        ({ p = p - 1, uv = uv - 1, n = -1 } :: vertices)

                                        Nothing ->
                                            case nComponents of
                                                nComponent :: _ ->
                                                    case String.toInt nComponent of
                                                        Just n ->
                                                            parseIndices more
                                                                ({ p = p - 1, uv = -1, n = n - 1 } :: vertices)

                                                        Nothing ->
                                                            []

                                                [] ->
                                                    parseIndices more
                                                        ({ p = p - 1, uv = -1, n = -1 } :: vertices)

                                [] ->
                                    parseIndices more
                                        ({ p = p - 1, uv = -1, n = -1 } :: vertices)

                        Nothing ->
                            []

                [] ->
                    []

        [] ->
            -- Note that this reverses vertices
            vertices


formatError : Int -> String -> Result String a
formatError lineno error =
    Err ("Line " ++ String.fromInt lineno ++ ": " ++ error)
