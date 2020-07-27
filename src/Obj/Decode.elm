module Obj.Decode exposing
    ( Decoder
    , triangles, faces, texturedTriangles, texturedFaces
    , decodeString
    , object, group, defaultGroup, material
    , objectNames, groupNames, materialNames
    , map, map2, map3, map4, map5
    , filter, oneOf, fail, succeed, andThen, combine
    , ObjCoordinates
    , facesIn, texturedFacesIn, texturedTrianglesIn, trianglesIn
    )

{-|

@docs Decoder


# Primitives

elm-obj-file currently supports triangular meshes that may have normal vectors and/or texture coordinates. Support for points and line segments may be added in a future release.

By default, the geometrical data is returned in the `ObjCoordinates` [coordinate system](https://github.com/ianmackenzie/elm-geometry#coordinate-systems).
It's also possible to [transform coordinates](#coordinate-conversion) if desired.

Note that all primitive decoders require at least one face and will fail if no faces are found.

@docs triangles, faces, texturedTriangles, texturedFaces


# Run Decoders

@docs decodeString


# Filtering

@docs object, group, defaultGroup, material


# Metadata

@docs objectNames, groupNames, materialNames


# Mapping

@docs map, map2, map3, map4, map5


# Advanced Decoding

@docs filter, oneOf, fail, succeed, andThen, combine


# Coordinate Conversion

@docs ObjCoordinates

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Length exposing (Length, Meters)
import Point3d exposing (Point3d)
import Quantity exposing (Unitless)
import Set
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


{-| A value that knows how to decode information from
[the OBJ file format](https://en.wikipedia.org/wiki/Wavefront_.obj_file)
-}
type Decoder a
    = Decoder (VertexData -> List ElementGroup -> Result String a)


{-| Decode just the plain positions. Use with `Scene3d.Mesh.indexedTriangles` and `Scene3d.Mesh.indexedFacets` from elm-3d-scene.
-}
triangles : Decoder (TriangularMesh (Point3d Meters ObjCoordinates))
triangles =
    trianglesIn Frame3d.atOrigin


{-| Decode positions and normals. Use with `Scene3d.Mesh.indexedFaces`.
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


{-| Decode positions, UV and normals. Use with `Scene3d.Mesh.texturedFaces`.
-}
texturedFaces : Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, normal : Vector3d Unitless ObjCoordinates, uv : ( Float, Float ) })
texturedFaces =
    texturedFacesIn Frame3d.atOrigin



-- RUN DECODERS


{-| Run the decoder on the string. Takes a function, that knows
how to convert float coordinates into physical units.

    decodeString Length.centimeters (texturedFaces []) string == Ok (TriangularMesh ...)
    decodeString Length.centimeters (texturedFaces []) string == Err ...

-}
decodeString : (Float -> Length) -> Decoder a -> String -> Result String a
decodeString units (Decoder decode) content =
    decodeHelp units decode (String.lines content) [] [] [] [] Nothing Nothing [ "default" ] []



-- FILTERING


{-| Decode the data for the object. You may store multiple objects in the same OBJ file.
Like the car base and car wheels.

    base =
        object "base" triangles

    wheels =
        object "wheels" triangles

-}
object : String -> Decoder a -> Decoder a
object name =
    filter (\properties -> properties.object == Just name)


{-| Decode the data for the certain group.
-}
group : String -> Decoder a -> Decoder a
group name =
    filter (\properties -> List.member name properties.groups)


{-| Decode the default group. This group has a special meaning,
all triangles\_ are assigned to it if a group is not specified.

    defaultGroup =
        group "default"

-}
defaultGroup : Decoder a -> Decoder a
defaultGroup =
    group "default"


{-| Decode the material.
-}
material : String -> Decoder a -> Decoder a
material name =
    filter (\properties -> properties.material == Just name)



-- METADATA


{-| Decode all object names within the current filter.
-}
objectNames : Decoder (List String)
objectNames =
    Decoder
        (\_ elements ->
            elements
                |> List.foldl
                    (\( groupProperties, _, _ ) objectsSet ->
                        case groupProperties.object of
                            Just obj ->
                                Set.insert obj objectsSet

                            Nothing ->
                                objectsSet
                    )
                    Set.empty
                |> Set.toList
                |> Result.Ok
        )


{-| Decode all group names within the current filter.
-}
groupNames : Decoder (List String)
groupNames =
    Decoder
        (\_ elements ->
            elements
                |> List.foldl
                    (\( groupProperties, _, _ ) groupsSet ->
                        List.foldl Set.insert groupsSet groupProperties.groups
                    )
                    Set.empty
                |> Set.toList
                |> Result.Ok
        )


{-| Decode all material names within the current filter.
-}
materialNames : Decoder (List String)
materialNames =
    Decoder
        (\_ elements ->
            elements
                |> List.foldl
                    (\( groupProperties, _, _ ) materialsSet ->
                        case groupProperties.material of
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


{-| Transform the decoder. May be useful in case need to post process the mesh data.
-}
map : (a -> b) -> Decoder a -> Decoder b
map fn (Decoder decoder) =
    Decoder (\vertexData elements -> Result.map fn (decoder vertexData elements))


{-| Join the result from two decoders. This lets you extract parts of the same OBJ file into separate meshes.

    carDecoder =
        map2 (\wheels base -> { wheels = wheels, base = base }) wheelsDecoder baseDecoder

-}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 fn (Decoder decoderA) (Decoder decoderB) =
    Decoder (\vertexData elements -> Result.map2 fn (decoderA vertexData elements) (decoderB vertexData elements))


{-| -}
map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 fn (Decoder decoderA) (Decoder decoderB) (Decoder decoderC) =
    Decoder (\vertexData elements -> Result.map3 fn (decoderA vertexData elements) (decoderB vertexData elements) (decoderC vertexData elements))


{-| -}
map4 : (a -> b -> c -> d -> e) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e
map4 fn (Decoder decoderA) (Decoder decoderB) (Decoder decoderC) (Decoder decoderD) =
    Decoder (\vertexData elements -> Result.map4 fn (decoderA vertexData elements) (decoderB vertexData elements) (decoderC vertexData elements) (decoderD vertexData elements))


{-| -}
map5 : (a -> b -> c -> d -> e -> f) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f
map5 fn (Decoder decoderA) (Decoder decoderB) (Decoder decoderC) (Decoder decoderD) (Decoder decoderE) =
    Decoder (\vertexData elements -> Result.map5 fn (decoderA vertexData elements) (decoderB vertexData elements) (decoderC vertexData elements) (decoderD vertexData elements) (decoderE vertexData elements))



-- ADVANCED DECODING


{-| Filter what should be decoded. For example, to implement the `group` decoder:

    group name =
        filter (\properties -> List.member name properties.groups)

-}
filter :
    ({ groups : List String, object : Maybe String, material : Maybe String } -> Bool)
    -> Decoder a
    -> Decoder a
filter fn (Decoder decoder) =
    Decoder
        (\vertexData elements ->
            decoder vertexData
                (List.filter
                    (\( properties, _, _ ) -> fn properties)
                    elements
                )
        )


{-| Try a bunch of different decoders. You will get the result from the first one that succeeds.
-}
oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    Decoder (\vertexData elements -> oneOfHelp vertexData elements decoders [])


oneOfHelp : VertexData -> List ElementGroup -> List (Decoder a) -> List String -> Result String a
oneOfHelp vertexData elements decoders errors =
    case decoders of
        [] ->
            if errors == [] then
                Err "Empty oneOf decoder"

            else
                Err ("Failed oneOf decoder: '" ++ String.join "', '" (List.reverse errors) ++ "'.")

        (Decoder decoder) :: remainingDecoders ->
            case decoder vertexData elements of
                Ok res ->
                    Ok res

                Err error ->
                    oneOfHelp vertexData elements remainingDecoders (error :: errors)


{-| A decoder that always succeeds with the result. May be useful in combination with `oneOf` to
provide a placeholder mesh if decoding fails.
-}
succeed : a -> Decoder a
succeed mesh =
    Decoder (\_ _ -> Result.Ok mesh)


{-| A decoder that always fails with a given error message.
Use it in case you need custom error messages.
-}
fail : String -> Decoder a
fail error =
    Decoder (\_ _ -> Result.Err error)


{-| Run one decoder and then run another decoder. Useful when you first want to look at metadata, and then filter based on that.
-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen fn (Decoder decoderA) =
    Decoder
        (\vertexData elements ->
            case decoderA vertexData elements of
                Ok result ->
                    case fn result of
                        Decoder decoderB ->
                            decoderB vertexData elements

                Err error ->
                    Err error
        )


{-| Combine multiple decoders together. An example use case is when you want
to extract meshes for all materials:

    -- Decode triangles for selected materials
    trianglesForMaterials names =
        names
            |> List.map
                (\name ->
                    map (\triangles -> ( name, triangles ))
                        (material name triangles)
                )
            |> combine

    -- Decode materials, and then decode triangles for these materials.
    withMaterials =
        materials |> andThen trianglesForMaterials

-}
combine : List (Decoder a) -> Decoder (List a)
combine decoders =
    Decoder
        (\vertexData elements ->
            combineHelp vertexData elements decoders []
        )


combineHelp : VertexData -> List ElementGroup -> List (Decoder a) -> List a -> Result String (List a)
combineHelp vertexData elements decoders list =
    case decoders of
        (Decoder decoder) :: remainingDecoders ->
            case decoder vertexData elements of
                Ok result ->
                    combineHelp vertexData elements remainingDecoders (result :: list)

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

    yUpToZUpFrame =
        Frame3d.atOrigin
            |> Frame3d.rotateAround Axis3d.x (Angle.degrees 90)

    yUpToZUpTriangles =
        trianglesIn yUpToZUpFrame

-}
trianglesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh (Point3d Meters coordinates))
trianglesIn frame =
    Decoder (\vertexData elements -> indexedTriangularMesh (addIndexedTriangles vertexData frame) elements initialIndexedState [])


{-| -}
facesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates })
facesIn frame =
    Decoder (\vertexData elements -> triangularMesh (addFaces vertexData frame) elements [])


{-| -}
texturedTrianglesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh { position : Point3d Meters coordinates, uv : ( Float, Float ) })
texturedTrianglesIn frame =
    Decoder (\vertexData elements -> triangularMesh (addTexturedTriangles vertexData frame) elements [])


{-| -}
texturedFacesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ) })
texturedFacesIn frame =
    Decoder (\vertexData elements -> triangularMesh (addTexturedFaces vertexData frame) elements [])



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


type alias GroupProperties =
    { groups : List String
    , object : Maybe String
    , material : Maybe String
    }


type alias VertexData =
    { positions : Array (Point3d Meters ObjCoordinates)
    , normals : Array (Direction3d ObjCoordinates)
    , uvs : Array ( Float, Float )
    }


type alias Element =
    List Vertex


type Vertex
    = Vertex Int (Maybe Int) (Maybe Int)


type alias ElementGroup =
    ( GroupProperties, Element, List Element )


type PropertyType
    = GroupsProperty (List String)
    | ObjectProperty String
    | MaterialProperty String


type Line
    = Property PropertyType
    | PositionData (Point3d Meters ObjCoordinates)
    | NormalData (Direction3d ObjCoordinates)
    | UvData ( Float, Float )
    | ElementData Element
    | Error String
    | Skip


{-| Defines a function that knows how to collect a certain kind of triangle
-}
type alias AddTriangles a =
    Vertex
    -> List Vertex
    -> List Element
    -> List ( a, a, a )
    -> Result String (List ( a, a, a ))


triangularMesh : AddTriangles a -> List ElementGroup -> List ( a, a, a ) -> Result String (TriangularMesh a)
triangularMesh add elementGroups triangles_ =
    case elementGroups of
        ( _, firstElement, remainingElements ) :: remainingElementGroups ->
            case firstElement of
                vertex :: remainingVertices ->
                    case add vertex remainingVertices remainingElements triangles_ of
                        Ok newTriangles ->
                            triangularMesh add remainingElementGroups newTriangles

                        Err error ->
                            Err error

                [] ->
                    Err "A face with less than three vertices"

        [] ->
            if triangles_ == [] then
                Err "No faces found"

            else
                Ok (TriangularMesh.triangles triangles_)


type alias IndexedState a =
    { maxIndex : Int
    , indexMap : Dict Int Int
    , vertices : List a
    }


initialIndexedState : IndexedState a
initialIndexedState =
    { maxIndex = -1
    , indexMap = Dict.empty
    , vertices = []
    }


{-| Defines a function that knows how to collect a certain kind of triangle
-}
type alias AddIndexedTriangles a =
    List Vertex
    -> List Element
    -> IndexedState a
    -> List Int
    -> List ( Int, Int, Int )
    -> Result String ( IndexedState a, List ( Int, Int, Int ) )


indexedTriangularMesh : AddIndexedTriangles a -> List ElementGroup -> IndexedState a -> List ( Int, Int, Int ) -> Result String (TriangularMesh a)
indexedTriangularMesh add elementGroups indexedState faceIndices =
    case elementGroups of
        ( _, firstElement, remainingElements ) :: remainingElementGroups ->
            case add firstElement remainingElements indexedState [] faceIndices of
                Ok ( newIndexedState, newFaceIndices ) ->
                    indexedTriangularMesh add remainingElementGroups newIndexedState newFaceIndices

                Err error ->
                    Err error

        [] ->
            if faceIndices == [] then
                Err "No faces found"

            else
                Ok (TriangularMesh.indexed (Array.fromList (List.reverse indexedState.vertices)) faceIndices)


addIndexedTriangles : VertexData -> Frame3d Meters coordinates { defines : ObjCoordinates } -> AddIndexedTriangles (Point3d Meters coordinates)
addIndexedTriangles vertexData frame vertices elements indexState indices faceIndices =
    case vertices of
        [] ->
            case indices of
                p1 :: remainingIndices ->
                    let
                        newFaceIndices =
                            groupIndices p1 remainingIndices faceIndices
                    in
                    case elements of
                        elementVertices :: remainingElements ->
                            addIndexedTriangles vertexData
                                frame
                                elementVertices
                                remainingElements
                                indexState
                                []
                                newFaceIndices

                        [] ->
                            Ok ( indexState, newFaceIndices )

                _ ->
                    -- the number of vertices is guaranteed in the parser
                    Ok ( indexState, faceIndices )

        (Vertex p _ _) :: remainingVertices ->
            case Dict.get p indexState.indexMap of
                Just idx ->
                    addIndexedTriangles vertexData
                        frame
                        remainingVertices
                        elements
                        indexState
                        (idx :: indices)
                        faceIndices

                Nothing ->
                    case Array.get p vertexData.positions of
                        Just vertex ->
                            addIndexedTriangles vertexData
                                frame
                                remainingVertices
                                elements
                                { maxIndex = indexState.maxIndex + 1
                                , indexMap = Dict.insert p (indexState.maxIndex + 1) indexState.indexMap
                                , vertices = Point3d.placeIn frame vertex :: indexState.vertices
                                }
                                (indexState.maxIndex + 1 :: indices)
                                faceIndices

                        Nothing ->
                            Err "Index out of range"


groupIndices : Int -> List Int -> List ( Int, Int, Int ) -> List ( Int, Int, Int )
groupIndices p1 more result =
    case more of
        p2 :: rest ->
            case rest of
                p3 :: _ ->
                    groupIndices p1 rest (( p1, p2, p3 ) :: result)

                _ ->
                    result

        [] ->
            result


addTriangles : VertexData -> Frame3d Meters coordinates { defines : ObjCoordinates } -> AddTriangles (Point3d Meters coordinates)
addTriangles vertexData frame firstVertex vertices elements triangles_ =
    case vertices of
        [ _ ] ->
            case elements of
                (newFirstVertex :: remainingVertices) :: remainingElements ->
                    addTriangles vertexData
                        frame
                        newFirstVertex
                        remainingVertices
                        remainingElements
                        triangles_

                _ ->
                    Ok triangles_

        (Vertex p2 _ _) :: remainingVertices ->
            case remainingVertices of
                (Vertex p3 _ _) :: _ ->
                    case firstVertex of
                        Vertex p1 _ _ ->
                            let
                                maybeV1 =
                                    Array.get p1 vertexData.positions

                                maybeV2 =
                                    Array.get p2 vertexData.positions

                                maybeV3 =
                                    Array.get p3 vertexData.positions
                            in
                            case
                                Maybe.map3
                                    (\v1 v2 v3 ->
                                        ( Point3d.placeIn frame v1
                                        , Point3d.placeIn frame v2
                                        , Point3d.placeIn frame v3
                                        )
                                    )
                                    maybeV1
                                    maybeV2
                                    maybeV3
                            of
                                Just triangle ->
                                    addTriangles vertexData frame firstVertex remainingVertices elements (triangle :: triangles_)

                                Nothing ->
                                    Err "Index out of range"

                _ ->
                    Err "A face with less than three vertices"

        _ ->
            Err "A face with less than three vertices"


addFaces : VertexData -> Frame3d Meters coordinates { defines : ObjCoordinates } -> AddTriangles (Face coordinates)
addFaces vertexData frame firstVertex vertices elements triangles_ =
    case vertices of
        [ _ ] ->
            case elements of
                (newFirstVertex :: remainingVertices) :: remainingElements ->
                    addFaces vertexData frame newFirstVertex remainingVertices remainingElements triangles_

                _ ->
                    Ok triangles_

        (Vertex p2 _ (Just n2)) :: remainingVertices ->
            case remainingVertices of
                (Vertex p3 _ (Just n3)) :: _ ->
                    case firstVertex of
                        Vertex p1 _ (Just n1) ->
                            let
                                maybeV1 =
                                    Maybe.map2
                                        (\position normal -> { position = Point3d.placeIn frame position, normal = Direction3d.toVector (Direction3d.placeIn frame normal) })
                                        (Array.get p1 vertexData.positions)
                                        (Array.get n1 vertexData.normals)

                                maybeV2 =
                                    Maybe.map2
                                        (\position normal -> { position = Point3d.placeIn frame position, normal = Direction3d.toVector (Direction3d.placeIn frame normal) })
                                        (Array.get p2 vertexData.positions)
                                        (Array.get n2 vertexData.normals)

                                maybeV3 =
                                    Maybe.map2
                                        (\position normal -> { position = Point3d.placeIn frame position, normal = Direction3d.toVector (Direction3d.placeIn frame normal) })
                                        (Array.get p3 vertexData.positions)
                                        (Array.get n3 vertexData.normals)
                            in
                            case Maybe.map3 (\v1 v2 v3 -> ( v1, v2, v3 )) maybeV1 maybeV2 maybeV3 of
                                Just triangle ->
                                    addFaces vertexData frame firstVertex remainingVertices elements (triangle :: triangles_)

                                Nothing ->
                                    Err "Index out of range"

                        _ ->
                            Err "Vertex has no normal vector"

                _ ->
                    Err "Vertex has no normal vector"

        _ ->
            Err "Vertex has no normal vector"


addTexturedTriangles : VertexData -> Frame3d Meters coordinates { defines : ObjCoordinates } -> AddTriangles (TexturedTriangle coordinates)
addTexturedTriangles vertexData frame firstVertex vertices elements triangles_ =
    case vertices of
        [ _ ] ->
            case elements of
                (newFirstVertex :: remainingVertices) :: remainingElements ->
                    addTexturedTriangles vertexData frame newFirstVertex remainingVertices remainingElements triangles_

                _ ->
                    Ok triangles_

        (Vertex p2 (Just uv2) _) :: remainingVertices ->
            case remainingVertices of
                (Vertex p3 (Just uv3) _) :: _ ->
                    case firstVertex of
                        Vertex p1 (Just uv1) _ ->
                            let
                                maybeV1 =
                                    Maybe.map2 (\position uv -> { position = Point3d.placeIn frame position, uv = uv })
                                        (Array.get p1 vertexData.positions)
                                        (Array.get uv1 vertexData.uvs)

                                maybeV2 =
                                    Maybe.map2 (\position uv -> { position = Point3d.placeIn frame position, uv = uv })
                                        (Array.get p2 vertexData.positions)
                                        (Array.get uv2 vertexData.uvs)

                                maybeV3 =
                                    Maybe.map2 (\position uv -> { position = Point3d.placeIn frame position, uv = uv })
                                        (Array.get p3 vertexData.positions)
                                        (Array.get uv3 vertexData.uvs)
                            in
                            case Maybe.map3 (\v1 v2 v3 -> ( v1, v2, v3 )) maybeV1 maybeV2 maybeV3 of
                                Just triangle ->
                                    addTexturedTriangles vertexData frame firstVertex remainingVertices elements (triangle :: triangles_)

                                Nothing ->
                                    Err "Index out of range"

                        _ ->
                            Err "Vertex has no texture coordinates"

                _ ->
                    Err "Vertex has no texture coordinates"

        _ ->
            Err "Vertex has no texture coordinates"


addTexturedFaces : VertexData -> Frame3d Meters coordinates { defines : ObjCoordinates } -> AddTriangles (TexturedFace coordinates)
addTexturedFaces vertexData frame firstVertex vertices elements triangles_ =
    case vertices of
        [ _ ] ->
            case elements of
                (newFirstVertex :: remainingVertices) :: remainingElements ->
                    addTexturedFaces vertexData frame newFirstVertex remainingVertices remainingElements triangles_

                _ ->
                    Ok triangles_

        (Vertex p2 (Just uv2) (Just n2)) :: remainingVertices ->
            case remainingVertices of
                (Vertex p3 (Just uv3) (Just n3)) :: _ ->
                    case firstVertex of
                        Vertex p1 (Just uv1) (Just n1) ->
                            let
                                maybeV1 =
                                    Maybe.map3
                                        (\position normal uv -> { position = Point3d.placeIn frame position, normal = Direction3d.toVector (Direction3d.placeIn frame normal), uv = uv })
                                        (Array.get p1 vertexData.positions)
                                        (Array.get n1 vertexData.normals)
                                        (Array.get uv1 vertexData.uvs)

                                maybeV2 =
                                    Maybe.map3
                                        (\position normal uv -> { position = Point3d.placeIn frame position, normal = Direction3d.toVector (Direction3d.placeIn frame normal), uv = uv })
                                        (Array.get p2 vertexData.positions)
                                        (Array.get n2 vertexData.normals)
                                        (Array.get uv2 vertexData.uvs)

                                maybeV3 =
                                    Maybe.map3
                                        (\position normal uv -> { position = Point3d.placeIn frame position, normal = Direction3d.toVector (Direction3d.placeIn frame normal), uv = uv })
                                        (Array.get p3 vertexData.positions)
                                        (Array.get n3 vertexData.normals)
                                        (Array.get uv3 vertexData.uvs)
                            in
                            case Maybe.map3 (\v1 v2 v3 -> ( v1, v2, v3 )) maybeV1 maybeV2 maybeV3 of
                                Just triangle ->
                                    addTexturedFaces vertexData frame firstVertex remainingVertices elements (triangle :: triangles_)

                                Nothing ->
                                    Err "Index out of range"

                        _ ->
                            Err "Vertex missing normal vector and/or texture coordinates"

                _ ->
                    Err "Vertex missing normal vector and/or texture coordinates"

        _ ->
            Err "Vertex missing normal vector and/or texture coordinates"


decodeHelp :
    (Float -> Length)
    -> (VertexData -> List ElementGroup -> Result String a)
    -> List String
    -> List (Point3d Meters ObjCoordinates)
    -> List (Direction3d ObjCoordinates)
    -> List ( Float, Float )
    -> List ElementGroup
    -> Maybe String
    -> Maybe String
    -> List String
    -> List Element
    -> Result String a
decodeHelp units decode lines positions normals uvs elementGroups object_ material_ groups_ elements =
    case lines of
        [] ->
            decode
                { positions = Array.fromList (List.reverse positions)
                , normals = Array.fromList (List.reverse normals)
                , uvs = Array.fromList (List.reverse uvs)
                }
                (case elements of
                    [] ->
                        elementGroups

                    element :: remainingElements ->
                        -- flush the last group
                        ( GroupProperties groups_ object_ material_, element, remainingElements ) :: elementGroups
                )

        line :: remainingLines ->
            case parseLine units line of
                Property propertyType ->
                    let
                        newElementGroups =
                            case elements of
                                [] ->
                                    elementGroups

                                element :: remainingElements ->
                                    ( GroupProperties groups_ object_ material_, element, remainingElements ) :: elementGroups
                    in
                    case propertyType of
                        GroupsProperty newGroups ->
                            decodeHelp units decode remainingLines positions normals uvs newElementGroups object_ material_ newGroups []

                        ObjectProperty newObject ->
                            decodeHelp units decode remainingLines positions normals uvs newElementGroups (Just newObject) material_ groups_ []

                        MaterialProperty newMaterial ->
                            decodeHelp units decode remainingLines positions normals uvs newElementGroups object_ (Just newMaterial) groups_ []

                PositionData position ->
                    decodeHelp units decode remainingLines (position :: positions) normals uvs elementGroups object_ material_ groups_ elements

                NormalData normal ->
                    decodeHelp units decode remainingLines positions (normal :: normals) uvs elementGroups object_ material_ groups_ elements

                UvData uv ->
                    decodeHelp units decode remainingLines positions normals (uv :: uvs) elementGroups object_ material_ groups_ elements

                ElementData element ->
                    decodeHelp units decode remainingLines positions normals uvs elementGroups object_ material_ groups_ (element :: elements)

                Skip ->
                    decodeHelp units decode remainingLines positions normals uvs elementGroups object_ material_ groups_ elements

                Error error ->
                    Err error


parseLine : (Float -> Length) -> String -> Line
parseLine units line =
    if String.startsWith "o " line then
        case String.trim (String.dropLeft 2 line) of
            "" ->
                Error "No object name"

            object_ ->
                Property (ObjectProperty object_)

    else if String.startsWith "g " line then
        case String.words (String.dropLeft 2 line) of
            [] ->
                Error "No groups specified"

            groups_ ->
                Property (GroupsProperty groups_)

    else if String.startsWith "usemtl " line then
        case String.trim (String.dropLeft 7 line) of
            "" ->
                Error "No material name"

            material_ ->
                Property (MaterialProperty material_)

    else if String.startsWith "v " line then
        case List.map String.toFloat (String.words (String.dropLeft 2 line)) of
            [ Just x, Just y, Just z ] ->
                PositionData (Point3d.xyz (units x) (units y) (units z))

            _ ->
                Error "Invalid position format"

    else if String.startsWith "vt " line then
        case List.map String.toFloat (String.words (String.dropLeft 3 line)) of
            [ Just x, Just y ] ->
                UvData ( x, y )

            _ ->
                Error "Invalid texture coordinates format"

    else if String.startsWith "vn " line then
        case List.map String.toFloat (String.words (String.dropLeft 3 line)) of
            [ Just x, Just y, Just z ] ->
                NormalData (Direction3d.unsafe { x = x, y = y, z = z })

            _ ->
                Error "Invalid normal vector format"

    else if String.startsWith "f " line then
        case parseVertices (String.words (String.dropLeft 2 line)) [] of
            Ok [] ->
                Error "Face has no vertices"

            Ok [ _ ] ->
                Error "Face has less than three vertices"

            Ok [ _, _ ] ->
                Error "Face has less than three vertices"

            Ok vertices ->
                ElementData vertices

            Err err ->
                Error err

    else
        Skip


parseVertices : List String -> List Vertex -> Result String (List Vertex)
parseVertices list vertices =
    case list of
        first :: more ->
            case List.map String.toInt (String.split "/" first) of
                [ Just p ] ->
                    parseVertices more
                        (Vertex (p - 1) Nothing Nothing :: vertices)

                [ Just p, Just uv ] ->
                    parseVertices more
                        (Vertex (p - 1) (Just (uv - 1)) Nothing :: vertices)

                [ Just p, uv, Just n ] ->
                    parseVertices more
                        (Vertex (p - 1) (Maybe.map (\x -> x - 1) uv) (Just (n - 1)) :: vertices)

                _ ->
                    Err "Invalid face format"

        [] ->
            Ok (List.reverse vertices)
