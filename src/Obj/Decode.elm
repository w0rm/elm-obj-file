module Obj.Decode exposing
    ( Decoder, triangles, faces, texturedTriangles, texturedFaces
    , decodeString
    , object, group, defaultGroup, material
    , objects, groups, materials
    , map, map2, map3, map4, map5
    , oneOf, fail, succeed, andThen, combine
    , ObjCoordinates
    )

{-|


# Primitives

@docs Decoder, triangles, faces, texturedTriangles, texturedFaces


# Run Decoders

@docs decodeString


# Filtering

@docs object, group, defaultGroup, material


# Metadata

@docs objects, groups, materials


# Mapping

@docs map, map2, map3, map4, map5


# Advanced Decoding

@docs filter, oneOf, fail, succeed, andThen, combine

@docs ObjCoordinates

-}

import Array exposing (Array)
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
    = Decoder (VertexData -> Elements -> Result String a)


{-| Decode just the plain positions. Use with `Scene3d.Mesh.indexedTriangles` and `Scene3d.Mesh.indexedFaces` from elm-3d-scene.
-}
triangles : Decoder (TriangularMesh (Point3d Meters ObjCoordinates))
triangles =
    Decoder (\vertexData elements -> collectTriangles (collectTriangle vertexData) elements [])


{-| Decode positions and normals. Use with `Scene3d.Mesh.indexedFaces`.
-}
faces : Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, normal : Vector3d Unitless ObjCoordinates })
faces =
    Decoder (\vertexData elements -> collectTriangles (collectFace vertexData) elements [])


{-| Decode positions and [UV](https://learnopengl.com/Getting-started/Textures) (texture) coordinates.
Use with `Scene3d.Mesh.texturedTriangles` or `Scene3d.Mesh.texturedFacets`.
-}
texturedTriangles : Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, uv : ( Float, Float ) })
texturedTriangles =
    Decoder (\vertexData elements -> collectTriangles (collectTexturedTriangle vertexData) elements [])


{-| Decode positions, UV and normals. Use with `Scene3d.Mesh.texturedFaces`.
-}
texturedFaces : Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, normal : Vector3d Unitless ObjCoordinates, uv : ( Float, Float ) })
texturedFaces =
    Decoder (\vertexData elements -> collectTriangles (collectTexturedFace vertexData) elements [])



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
all polygons are assigned to it if a group is not specified.

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


{-| Decode all the objects.
-}
objects : Decoder (List String)
objects =
    Decoder
        (\_ elements ->
            elements
                |> List.foldl
                    (\( groupProperties, _ ) objectsSet ->
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


{-| Decode all the groups.
-}
groups : Decoder (List String)
groups =
    Decoder
        (\_ elements ->
            elements
                |> List.foldl
                    (\( groupProperties, _ ) groupsSet ->
                        List.foldl Set.insert groupsSet groupProperties.groups
                    )
                    Set.empty
                |> Set.toList
                |> Result.Ok
        )


{-| Decode all the materials.
-}
materials : Decoder (List String)
materials =
    Decoder
        (\_ elements ->
            elements
                |> List.foldl
                    (\( groupProperties, _ ) materialsSet ->
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
                    (\( properties, _ ) -> fn properties)
                    elements
                )
        )


{-| Try a bunch of different decoders. You will get the result from the first one that succeeds.
-}
oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    Decoder (\vertexData elements -> oneOfHelp vertexData elements decoders)


oneOfHelp : VertexData -> Elements -> List (Decoder a) -> Result String a
oneOfHelp vertexData elements decoders =
    case decoders of
        [] ->
            Err "Failed oneOf"

        (Decoder decoder) :: remainingDecoders ->
            case decoder vertexData elements of
                Ok res ->
                    Ok res

                Err _ ->
                    oneOfHelp vertexData elements remainingDecoders


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


combineHelp : VertexData -> Elements -> List (Decoder a) -> List a -> Result String (List a)
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



-- Internals


type alias Face =
    { position : Point3d Meters ObjCoordinates
    , normal : Vector3d Unitless ObjCoordinates
    }


type alias TexturedTriangle =
    { position : Point3d Meters ObjCoordinates
    , uv : ( Float, Float )
    }


type alias TexturedFace =
    { position : Point3d Meters ObjCoordinates
    , normal : Vector3d Unitless ObjCoordinates
    , uv : ( Float, Float )
    }


type alias GroupProperties =
    { groups : List String
    , object : Maybe String
    , material : Maybe String
    }


type alias VertexData =
    { positions : Array (Point3d Meters ObjCoordinates)
    , normals : Array (Vector3d Unitless ObjCoordinates)
    , uvs : Array ( Float, Float )
    }


type alias Elements =
    List ( GroupProperties, List (List (List (Maybe Int))) )


type PropertyType
    = GroupsProperty (List String)
    | ObjectProperty String
    | MaterialProperty String


type Line
    = Property PropertyType
    | PositionData (Point3d Meters ObjCoordinates)
    | NormalData (Vector3d Unitless ObjCoordinates)
    | UvData ( Float, Float )
    | FaceIndices (List (List (Maybe Int)))
    | Error String
    | Skip


collectTriangles : CollectTriangle a -> Elements -> List ( a, a, a ) -> Result String (TriangularMesh a)
collectTriangles collect elements polygons =
    case elements of
        ( _, elementIndices ) :: remainingElements ->
            case elementIndices of
                (first :: indices) :: remainingElementIndices ->
                    case collect first indices remainingElementIndices polygons of
                        Ok newPolygons ->
                            collectTriangles collect remainingElements newPolygons

                        Err error ->
                            Err error

                _ ->
                    Err "Empty number of indices in a group"

        [] ->
            if polygons == [] then
                Err "Not found"

            else
                Ok (TriangularMesh.triangles polygons)


{-| Defines a function that knows how to collect a certain kind of triangle
-}
type alias CollectTriangle a =
    List (Maybe Int)
    -> List (List (Maybe Int))
    -> List (List (List (Maybe Int)))
    -> List ( a, a, a )
    -> Result String (List ( a, a, a ))


collectTriangle : VertexData -> CollectTriangle (Point3d Meters ObjCoordinates)
collectTriangle vertexData firstVertex vertices elementIndices polygons =
    case vertices of
        [ _ ] ->
            case elementIndices of
                [] ->
                    Ok polygons

                (first :: indices) :: remainingElementIndices ->
                    collectTriangle vertexData first indices remainingElementIndices polygons

                _ ->
                    Err "Missing indices"

        ((Just p2) :: _) :: remainingVertices ->
            case remainingVertices of
                ((Just p3) :: _) :: _ ->
                    case firstVertex of
                        (Just p1) :: _ ->
                            let
                                maybeV1 =
                                    Array.get (p1 - 1) vertexData.positions

                                maybeV2 =
                                    Array.get (p2 - 1) vertexData.positions

                                maybeV3 =
                                    Array.get (p3 - 1) vertexData.positions
                            in
                            case Maybe.map3 (\v1 v2 v3 -> ( v1, v2, v3 )) maybeV1 maybeV2 maybeV3 of
                                Just triangle ->
                                    collectTriangle vertexData firstVertex remainingVertices elementIndices (triangle :: polygons)

                                Nothing ->
                                    Err "Missing indices"

                        _ ->
                            Err "Missing indices"

                _ ->
                    Err "Missing indices"

        _ ->
            Err "Missing indices"


collectFace : VertexData -> CollectTriangle Face
collectFace vertexData firstVertex vertices elementIndices polygons =
    case vertices of
        [ _ ] ->
            case elementIndices of
                [] ->
                    Ok polygons

                (first :: indices) :: remainingElementIndices ->
                    collectFace vertexData first indices remainingElementIndices polygons

                _ ->
                    Err "Missing indices"

        [ Just p2, _, Just n2 ] :: remainingVertices ->
            case remainingVertices of
                [ Just p3, _, Just n3 ] :: _ ->
                    case firstVertex of
                        [ Just p1, _, Just n1 ] ->
                            let
                                maybeV1 =
                                    Maybe.map2 Face (Array.get (p1 - 1) vertexData.positions) (Array.get (n1 - 1) vertexData.normals)

                                maybeV2 =
                                    Maybe.map2 Face (Array.get (p2 - 1) vertexData.positions) (Array.get (n2 - 1) vertexData.normals)

                                maybeV3 =
                                    Maybe.map2 Face (Array.get (p3 - 1) vertexData.positions) (Array.get (n3 - 1) vertexData.normals)
                            in
                            case Maybe.map3 (\v1 v2 v3 -> ( v1, v2, v3 )) maybeV1 maybeV2 maybeV3 of
                                Just triangle ->
                                    collectFace vertexData firstVertex remainingVertices elementIndices (triangle :: polygons)

                                Nothing ->
                                    Err "Missing indices"

                        _ ->
                            Err "Missing indices"

                _ ->
                    Err "Missing indices"

        _ ->
            Err "Missing indices"


collectTexturedTriangle : VertexData -> CollectTriangle TexturedTriangle
collectTexturedTriangle vertexData firstVertex vertices elementIndices polygons =
    case vertices of
        [ _ ] ->
            case elementIndices of
                [] ->
                    Ok polygons

                (first :: indices) :: remainingElementIndices ->
                    collectTexturedTriangle vertexData first indices remainingElementIndices polygons

                _ ->
                    Err "Missing indices"

        ((Just p2) :: (Just uv2) :: _) :: remainingVertices ->
            case remainingVertices of
                ((Just p3) :: (Just uv3) :: _) :: _ ->
                    case firstVertex of
                        (Just p1) :: (Just uv1) :: _ ->
                            let
                                maybeV1 =
                                    Maybe.map2 TexturedTriangle (Array.get (p1 - 1) vertexData.positions) (Array.get (uv1 - 1) vertexData.uvs)

                                maybeV2 =
                                    Maybe.map2 TexturedTriangle (Array.get (p2 - 1) vertexData.positions) (Array.get (uv2 - 1) vertexData.uvs)

                                maybeV3 =
                                    Maybe.map2 TexturedTriangle (Array.get (p3 - 1) vertexData.positions) (Array.get (uv3 - 1) vertexData.uvs)
                            in
                            case Maybe.map3 (\v1 v2 v3 -> ( v1, v2, v3 )) maybeV1 maybeV2 maybeV3 of
                                Just triangle ->
                                    collectTexturedTriangle vertexData firstVertex remainingVertices elementIndices (triangle :: polygons)

                                Nothing ->
                                    Err "Missing indices"

                        _ ->
                            Err "Missing indices"

                _ ->
                    Err "Missing indices"

        _ ->
            Err "Missing indices"


collectTexturedFace : VertexData -> CollectTriangle TexturedFace
collectTexturedFace vertexData firstVertex vertices elementIndices polygons =
    case vertices of
        [ _ ] ->
            case elementIndices of
                [] ->
                    Ok polygons

                (first :: indices) :: remainingElementIndices ->
                    collectTexturedFace vertexData first indices remainingElementIndices polygons

                _ ->
                    Err "Missing indices"

        [ Just p2, Just uv2, Just n2 ] :: remainingVertices ->
            case remainingVertices of
                [ Just p3, Just uv3, Just n3 ] :: _ ->
                    case firstVertex of
                        [ Just p1, Just uv1, Just n1 ] ->
                            let
                                maybeV1 =
                                    Maybe.map3 TexturedFace (Array.get (p1 - 1) vertexData.positions) (Array.get (n1 - 1) vertexData.normals) (Array.get (uv1 - 1) vertexData.uvs)

                                maybeV2 =
                                    Maybe.map3 TexturedFace (Array.get (p2 - 1) vertexData.positions) (Array.get (n2 - 1) vertexData.normals) (Array.get (uv2 - 1) vertexData.uvs)

                                maybeV3 =
                                    Maybe.map3 TexturedFace (Array.get (p3 - 1) vertexData.positions) (Array.get (n3 - 1) vertexData.normals) (Array.get (uv3 - 1) vertexData.uvs)
                            in
                            case Maybe.map3 (\v1 v2 v3 -> ( v1, v2, v3 )) maybeV1 maybeV2 maybeV3 of
                                Just triangle ->
                                    collectTexturedFace vertexData firstVertex remainingVertices elementIndices (triangle :: polygons)

                                Nothing ->
                                    Err "Missing indices"

                        _ ->
                            Err "Missing indices"

                _ ->
                    Err "Missing indices"

        _ ->
            Err "Missing indices"


decodeHelp :
    (Float -> Length)
    -> (VertexData -> Elements -> Result String a)
    -> List String
    -> List (Point3d Meters ObjCoordinates)
    -> List (Vector3d Unitless ObjCoordinates)
    -> List ( Float, Float )
    -> List ( GroupProperties, List (List (List (Maybe Int))) )
    -> Maybe String
    -> Maybe String
    -> List String
    -> List (List (List (Maybe Int)))
    -> Result String a
decodeHelp units decode lines positions normals uvs elements object_ material_ groups_ indices =
    case lines of
        [] ->
            decode
                { positions = Array.fromList (List.reverse positions)
                , normals = Array.fromList (List.reverse normals)
                , uvs = Array.fromList (List.reverse uvs)
                }
                (if indices == [] then
                    elements

                 else
                    -- flush the last group of face indices
                    ( GroupProperties groups_ object_ material_, indices ) :: elements
                )

        line :: remainingLines ->
            case parseLine units line of
                Property propertyType ->
                    let
                        newFaces =
                            if indices == [] then
                                elements

                            else
                                ( GroupProperties groups_ object_ material_, indices ) :: elements
                    in
                    case propertyType of
                        GroupsProperty newGroups ->
                            decodeHelp units decode remainingLines positions normals uvs newFaces object_ material_ newGroups []

                        ObjectProperty newObject ->
                            decodeHelp units decode remainingLines positions normals uvs newFaces (Just newObject) material_ groups_ []

                        MaterialProperty newMaterial ->
                            decodeHelp units decode remainingLines positions normals uvs newFaces object_ (Just newMaterial) groups_ []

                PositionData position ->
                    decodeHelp units decode remainingLines (position :: positions) normals uvs elements object_ material_ groups_ indices

                NormalData normal ->
                    decodeHelp units decode remainingLines positions (normal :: normals) uvs elements object_ material_ groups_ indices

                UvData uv ->
                    decodeHelp units decode remainingLines positions normals (uv :: uvs) elements object_ material_ groups_ indices

                FaceIndices faceIndices ->
                    decodeHelp units decode remainingLines positions normals uvs elements object_ material_ groups_ (faceIndices :: indices)

                Skip ->
                    decodeHelp units decode remainingLines positions normals uvs elements object_ material_ groups_ indices

                Error error ->
                    Err error


parseLine : (Float -> Length) -> String -> Line
parseLine units line =
    if String.startsWith "o " line then
        case String.trim (String.dropLeft 2 line) of
            "" ->
                Error "Expect object name"

            object_ ->
                Property (ObjectProperty object_)

    else if String.startsWith "g " line then
        case String.words (String.dropLeft 2 line) of
            [] ->
                Error "Expect group name"

            groups_ ->
                Property (GroupsProperty groups_)

    else if String.startsWith "usemtl " line then
        case String.trim (String.dropLeft 7 line) of
            "" ->
                Error "Expect material name"

            material_ ->
                Property (MaterialProperty material_)

    else if String.startsWith "v " line then
        case List.map String.toFloat (String.words (String.dropLeft 2 line)) of
            [ Just x, Just y, Just z ] ->
                PositionData (Point3d.xyz (units x) (units y) (units z))

            _ ->
                Error "Invalid position data"

    else if String.startsWith "vt " line then
        case List.map String.toFloat (String.words (String.dropLeft 3 line)) of
            [ Just x, Just y ] ->
                UvData ( x, y )

            _ ->
                Error "Invalid uv data"

    else if String.startsWith "vn " line then
        case List.map String.toFloat (String.words (String.dropLeft 3 line)) of
            [ Just x, Just y, Just z ] ->
                NormalData (Vector3d.unitless x y z)

            _ ->
                Error "Invalid normal data"

    else if String.startsWith "f " line then
        case String.words (String.dropLeft 2 line) of
            [] ->
                Error "Expect face indices"

            faceIndices ->
                FaceIndices (List.map (String.split "/" >> List.map String.toInt) faceIndices)

    else
        Skip
