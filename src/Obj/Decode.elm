module Obj.Decode exposing
    ( Decoder, triangles, faces, texturedTriangles, texturedFaces
    , decodeString
    , Filter, object, group, defaultGroup, material
    , objects, groups, materials
    , map, map2, map3, map4, map5
    , oneOf, fail, succeed
    , ObjCoordinates
    )

{-|


# Primitives

@docs Decoder, triangles, faces, texturedTriangles, texturedFaces


# Run Decoders

@docs decodeString


# Filtering

@docs Filter, object, group, defaultGroup, material


# Metadata

@docs objects, groups, materials


# Mapping

@docs map, map2, map3, map4, map5


# Advanced Decoding

@docs oneOf, fail, succeed

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
    fail "Implement triangles"


{-| Decode positions and normals. Use with `Scene3d.Mesh.indexedFaces`.
-}
faces : Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, normal : Vector3d Unitless ObjCoordinates })
faces =
    fail "Implement faces"


{-| Decode positions and [UV](https://learnopengl.com/Getting-started/Textures) (texture) coordinates.
Use with `Scene3d.Mesh.texturedTriangles` or `Scene3d.Mesh.texturedFacets`.
-}
texturedTriangles : Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, uv : ( Float, Float ) })
texturedTriangles =
    fail "Implement texturedTriangles"


{-| Decode positions, UV and normals. Use with `Scene3d.Mesh.texturedFaces`.
-}
texturedFaces : Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, normal : Vector3d Unitless ObjCoordinates, uv : ( Float, Float ) })
texturedFaces =
    Decoder
        (\vertexData elements ->
            if elements == [] then
                Err "Not found"

            else
                collectTexturedFaces vertexData elements []
        )


{-| Run the decoder on the string. Takes a function, that knows
how to convert float coordinates into physical units.

    decodeString Length.centimeters (texturedFaces []) string == Ok (TriangularMesh ...)
    decodeString Length.centimeters (texturedFaces []) string == Err ...

-}
decodeString : (Float -> Length) -> Decoder a -> String -> Result String a
decodeString units (Decoder decode) content =
    parse units decode content


{-| All the primitive decodes take a list of filters. This lets you select only the subset of polygons from the OBJ file.
All filters in the list are joined with the `and`.
-}
type Filter
    = Group String
    | Material String
    | Object String


{-| Filter polygons that belong to a certain group.
-}
group : String -> Filter
group =
    Group


{-| Filter by default group. This group has a special meaning,
all polygons are assigned to it if a group is not specified.

    defaultGroup =
        group "default"

-}
defaultGroup : Filter
defaultGroup =
    Group "default"


{-| Filter by object. You may store multiple objects in the same OBJ file. Like a car base and car wheels.
-}
object : String -> Filter
object =
    Object


{-| Filter by material name.
-}
material : String -> Filter
material =
    Material


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


{-| Try a bunch of different decoders. You will get the result from the first one that succeeds.
-}
oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    Decoder (\vertexData elements -> oneOfHelp vertexData elements decoders)


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



-- Internals


type alias TexturedFace =
    { position : Point3d Meters ObjCoordinates
    , normal : Vector3d Unitless ObjCoordinates
    , uv : ( Float, Float )
    }


type ObjCoordinates
    = ObjCoordinates


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


collectTexturedFaces : VertexData -> Elements -> List ( TexturedFace, TexturedFace, TexturedFace ) -> Result String (TriangularMesh TexturedFace)
collectTexturedFaces vertexData elements polygons =
    case elements of
        ( _, elementIndices ) :: remainingElements ->
            case elementIndices of
                (first :: indices) :: remainingElementIndices ->
                    case addTexturedFacesPolygons vertexData first indices remainingElementIndices polygons of
                        Ok newPolygons ->
                            collectTexturedFaces vertexData remainingElements newPolygons

                        Err error ->
                            Err error

                _ ->
                    Err "Empty number of indices in a group"

        [] ->
            Ok (TriangularMesh.triangles polygons)


addTexturedFacesPolygons : VertexData -> List (Maybe Int) -> List (List (Maybe Int)) -> List (List (List (Maybe Int))) -> List ( TexturedFace, TexturedFace, TexturedFace ) -> Result String (List ( TexturedFace, TexturedFace, TexturedFace ))
addTexturedFacesPolygons vertexData firstVertex vertices elementIndices polygons =
    case vertices of
        _ :: [] ->
            case elementIndices of
                [] ->
                    Ok polygons

                (first :: indices) :: remainingElementIndices ->
                    addTexturedFacesPolygons vertexData first indices remainingElementIndices polygons

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
                                    addTexturedFacesPolygons
                                        vertexData
                                        firstVertex
                                        remainingVertices
                                        elementIndices
                                        (triangle :: polygons)

                                Nothing ->
                                    Err "Missing indices"

                        _ ->
                            Err "Missing indices"

                _ ->
                    Err "Missing indices"

        _ ->
            Err "Missing indices"


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


parse : (Float -> Length) -> (VertexData -> Elements -> Result String a) -> String -> Result String a
parse units decode content =
    parseHelp units decode (String.lines content) [] [] [] [] Nothing Nothing [ "default" ] []


parseHelp :
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
parseHelp units decode lines positions normals uvs elements object_ material_ groups_ indices =
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
                    ( { groups = groups_, object = object_, material = material_ }, indices ) :: elements
                )

        line :: remainingLines ->
            case parseLine units line of
                Property propertyType ->
                    let
                        newFaces =
                            if indices == [] then
                                elements

                            else
                                ( { groups = groups_, object = object_, material = material_ }, indices ) :: elements
                    in
                    case propertyType of
                        GroupsProperty newGroups ->
                            parseHelp units decode remainingLines positions normals uvs newFaces object_ material_ newGroups []

                        ObjectProperty newObject ->
                            parseHelp units decode remainingLines positions normals uvs newFaces (Just newObject) material_ groups_ []

                        MaterialProperty newMaterial ->
                            parseHelp units decode remainingLines positions normals uvs newFaces object_ (Just newMaterial) groups_ []

                PositionData position ->
                    parseHelp units decode remainingLines (position :: positions) normals uvs elements object_ material_ groups_ indices

                NormalData normal ->
                    parseHelp units decode remainingLines positions (normal :: normals) uvs elements object_ material_ groups_ indices

                UvData uv ->
                    parseHelp units decode remainingLines positions normals (uv :: uvs) elements object_ material_ groups_ indices

                FaceIndices faceIndices ->
                    parseHelp units decode remainingLines positions normals uvs elements object_ material_ groups_ (faceIndices :: indices)

                Skip ->
                    parseHelp units decode remainingLines positions normals uvs elements object_ material_ groups_ indices

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


matches : List Filter -> GroupProperties -> Bool
matches filters properties =
    List.all
        (\filter ->
            case filter of
                Group group_ ->
                    List.member group_ properties.groups

                Object object_ ->
                    properties.object == Just object_

                Material material_ ->
                    properties.material == Just material_
        )
        filters
