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
    , bitflagFacesIn, bitflagTexturedFacesIn, bitflagBumpyFacesIn
    )

{-|

@docs Decoder


# Primitives

elm-obj-file supports triangular meshes that may have normal vectors and/or texture coordinates, polylines and points.

By default, the geometrical data is returned in the `ObjCoordinates` [coordinate system](https://github.com/ianmackenzie/elm-geometry#coordinate-systems).
It's also possible to [transform coordinates](#coordinate-conversion) if desired.

Note that all primitive decoders require at least one element and will fail if no elements are found.

For `faces`, `texturedFaces`, and `bumpyFaces`, normal vectors are taken directly from the mesh when present.
For faces that lack explicit normals, they are computed from the file’s smoothing groups — see [Blender Bitflag Smooth Groups](#blender-bitflag-smooth-groups) for details.

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


# Blender Bitflag Smooth Groups

OBJ files can assign faces to smoothing groups with `s N` directives. Faces in the same group share smooth normals at shared vertices — normals are computed by averaging the weighted face normals of the group. Faces with `s 0` or `s off` get flat normals.

By default, two faces are in the same group when their IDs are equal. Blender's "Smooth Group Bitflags" export option uses a different encoding: IDs are bitflags, and two faces are in the same group when their IDs share any bits (`a & b /= 0`). Use these decoders for such files.

@docs bitflagFacesIn, bitflagTexturedFacesIn, bitflagBumpyFacesIn

-}

import Array exposing (Array)
import Frame3d exposing (Frame3d)
import Http
import Length exposing (Length, Meters)
import Obj.Internal.Faces as Faces
import Obj.Internal.Parse as Parse exposing (Group(..), LineElement(..), PointsElement(..), Vertex, VertexData, formatError)
import Obj.Internal.Triangles as Triangles
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import Quantity exposing (Unitless)
import Set
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


{-| A value that knows how to decode information from
[the OBJ file format](https://en.wikipedia.org/wiki/Wavefront_.obj_file)
-}
type Decoder a
    = Decoder (VertexData ObjCoordinates -> List String -> List Group -> Result String a)


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
    case Parse.parse unitsFn content of
        Ok ( vertexData, groups ) ->
            decode vertexData [] groups

        Err err ->
            Err err


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


{-| Transform the decoder. For example, if you need to decode triangles' vertices:

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
filter fn =
    filterHelp "<custom filter>"
        (\properties -> fn { groups = properties.groups, object = properties.object, material = properties.material })


filterHelp :
    String
    -> ({ groups : List String, object : Maybe String, material : Maybe String, smoothingGroup : Int } -> Bool)
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


oneOfHelp : VertexData ObjCoordinates -> List String -> List Group -> List (Decoder a) -> List String -> Result String a
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


combineHelp : VertexData ObjCoordinates -> List String -> List Group -> List (Decoder a) -> List a -> Result String (List a)
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
    = ObjCoordinates Never


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
    Decoder (Triangles.triangles frame)


{-| -}
facesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates })
facesIn frame =
    Decoder (Faces.faces frame False)


{-| -}
texturedTrianglesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh { position : Point3d Meters coordinates, uv : ( Float, Float ) })
texturedTrianglesIn frame =
    Decoder (Triangles.texturedTriangles frame)


{-| -}
texturedFacesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ) })
texturedFacesIn frame =
    Decoder (Faces.texturedFaces frame False)


{-| -}
bumpyFacesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ), tangent : Vector3d Unitless coordinates, tangentBasisIsRightHanded : Bool })
bumpyFacesIn frame =
    Decoder (Faces.bumpyFaces frame False)


{-| -}
bitflagFacesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates })
bitflagFacesIn frame =
    Decoder (Faces.faces frame True)


{-| -}
bitflagTexturedFacesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ) })
bitflagTexturedFacesIn frame =
    Decoder (Faces.texturedFaces frame True)


{-| -}
bitflagBumpyFacesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ), tangent : Vector3d Unitless coordinates, tangentBasisIsRightHanded : Bool })
bitflagBumpyFacesIn frame =
    Decoder (Faces.bumpyFaces frame True)


{-| -}
polylinesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (List (Polyline3d Meters coordinates))
polylinesIn frame =
    Decoder
        (\vertexData filters groups ->
            polylinesHelp vertexData.positions frame filters groups [] 0 [] [] []
        )


polylinesHelp :
    Array (Point3d Meters ObjCoordinates)
    -> Frame3d Meters coordinates { defines : ObjCoordinates }
    -> List String
    -> List Group
    -> List LineElement
    -> Int
    -> List Vertex
    -> List (Point3d Meters coordinates)
    -> List (Polyline3d Meters coordinates)
    -> Result String (List (Polyline3d Meters coordinates))
polylinesHelp positions frame filters groups elements lineno vertices points_ result =
    case vertices of
        { p } :: remainingVertices ->
            case Array.get p positions of
                Just point ->
                    polylinesHelp positions
                        frame
                        filters
                        groups
                        elements
                        lineno
                        remainingVertices
                        (Point3d.placeIn frame point :: points_)
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
                    polylinesHelp positions
                        frame
                        filters
                        groups
                        remainingElements
                        newLineno
                        newVertices
                        []
                        newResult

                [] ->
                    case groups of
                        (Group _ _ newElements _) :: remainingGroups ->
                            polylinesHelp positions
                                frame
                                filters
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
                                    case filters of
                                        _ :: _ ->
                                            Err ("No lines found for " ++ String.join ", " filters)

                                        [] ->
                                            Err "No lines found"


{-| -}
pointsIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (List (Point3d Meters coordinates))
pointsIn frame =
    Decoder
        (\vertexData filters groups ->
            pointsHelp vertexData.positions frame filters groups [] 0 [] []
        )


pointsHelp :
    Array (Point3d Meters ObjCoordinates)
    -> Frame3d Meters coordinates { defines : ObjCoordinates }
    -> List String
    -> List Group
    -> List PointsElement
    -> Int
    -> List Vertex
    -> List (Point3d Meters coordinates)
    -> Result String (List (Point3d Meters coordinates))
pointsHelp positions frame filters groups elements lineno vertices result =
    case vertices of
        { p } :: remainingVertices ->
            case Array.get p positions of
                Just point ->
                    pointsHelp positions
                        frame
                        filters
                        groups
                        elements
                        lineno
                        remainingVertices
                        (Point3d.placeIn frame point :: result)

                Nothing ->
                    formatError lineno "Index out of range"

        [] ->
            case elements of
                (PointsElement newLineno newVertices) :: remainingElements ->
                    pointsHelp positions
                        frame
                        filters
                        groups
                        remainingElements
                        newLineno
                        newVertices
                        result

                [] ->
                    case groups of
                        (Group _ _ _ newElements) :: remainingGroups ->
                            pointsHelp positions
                                frame
                                filters
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
                                    case filters of
                                        _ :: _ ->
                                            Err ("No points found for " ++ String.join ", " filters)

                                        [] ->
                                            Err "No points found"
