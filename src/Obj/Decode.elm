module Obj.Decode exposing
    ( Decoder, triangles, faces, texturedTriangles, texturedFaces
    , Filter, object, group, defaultGroup, material
    , decodeString
    , map, map2, map3, map4, map5
    , oneOf, fail, succeed
    , ObjCoordinates
    )

{-|


# Primitives

@docs Decoder, triangles, faces, texturedTriangles, texturedFaces


# Filtering

@docs Filter, object, group, defaultGroup, material


# Run Decoders

@docs decodeString


# Mapping

@docs map, map2, map3, map4, map5


# Advanced Decoding

@docs oneOf, fail, succeed

-}

import Array exposing (Array)
import Length exposing (Length, Meters)
import Point3d exposing (Point3d)
import Quantity exposing (Unitless)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


{-| A value that knows how to decode information from
[the OBJ file format](https://en.wikipedia.org/wiki/Wavefront_.obj_file)
-}
type Decoder a
    = Decoder (ParsedFile -> Result String a)


{-| Decode just the plain positions. Use with `Scene3d.Mesh.indexedTriangles` and `Scene3d.Mesh.indexedFaces` from elm-3d-scene.
-}
triangles : List Filter -> Decoder (TriangularMesh (Point3d Meters ObjCoordinates))
triangles filters =
    fail "Implement triangles"


{-| Decode positions and normals. Use with `Scene3d.Mesh.indexedFaces`.
-}
faces : List Filter -> Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, normal : Vector3d Unitless ObjCoordinates })
faces filters =
    fail "Implement faces"


{-| Decode positions and [UV](https://learnopengl.com/Getting-started/Textures) (texture) coordinates.
Use with `Scene3d.Mesh.texturedTriangles` or `Scene3d.Mesh.texturedFacets`.
-}
texturedTriangles : List Filter -> Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, uv : ( Float, Float ) })
texturedTriangles filters =
    fail "Implement texturedTriangles"


{-| Decode positions, UV and normals. Use with `Scene3d.Mesh.texturedFaces`.
-}
texturedFaces : List Filter -> Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, normal : Vector3d Unitless ObjCoordinates, uv : ( Float, Float ) })
texturedFaces filters =
    Decoder
        (\parsedFile ->
            case List.filter (Tuple.first >> matches filters) parsedFile.faces of
                [] ->
                    Err "Not found"

                filteredFaces ->
                    collectTexturedFaces parsedFile filteredFaces []
        )


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


{-| Run the decoder on the string. Takes a function, that knows
how to convert float coordinates into physical units.

    decodeString Length.centimeters (texturedFaces []) string == Ok (TriangularMesh ...)
    decodeString Length.centimeters (texturedFaces []) string == Err ...

-}
decodeString : (Float -> Length) -> Decoder a -> String -> Result String a
decodeString units (Decoder decode) content =
    content
        |> parse units
        |> Result.andThen decode


{-| Transform the decoder. May be useful in case need to post process the mesh data.
-}
map : (a -> b) -> Decoder a -> Decoder b
map fn (Decoder decoder) =
    Decoder (\parsedFile -> Result.map fn (decoder parsedFile))


{-| Join the result from two decoders. This lets you extract parts of the same OBJ file into separate meshes.

    carDecoder =
        map2 (\wheels base -> { wheels = wheels, base = base }) wheelsDecoder baseDecoder

-}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 fn (Decoder decoderA) (Decoder decoderB) =
    Decoder (\parsedFile -> Result.map2 fn (decoderA parsedFile) (decoderB parsedFile))


{-| -}
map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 fn (Decoder decoderA) (Decoder decoderB) (Decoder decoderC) =
    Decoder (\parsedFile -> Result.map3 fn (decoderA parsedFile) (decoderB parsedFile) (decoderC parsedFile))


{-| -}
map4 : (a -> b -> c -> d -> e) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e
map4 fn (Decoder decoderA) (Decoder decoderB) (Decoder decoderC) (Decoder decoderD) =
    Decoder (\parsedFile -> Result.map4 fn (decoderA parsedFile) (decoderB parsedFile) (decoderC parsedFile) (decoderD parsedFile))


{-| -}
map5 : (a -> b -> c -> d -> e -> f) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f
map5 fn (Decoder decoderA) (Decoder decoderB) (Decoder decoderC) (Decoder decoderD) (Decoder decoderE) =
    Decoder (\parsedFile -> Result.map5 fn (decoderA parsedFile) (decoderB parsedFile) (decoderC parsedFile) (decoderD parsedFile) (decoderE parsedFile))


{-| Try a bunch of different decoders. You will get the result from the first one that succeeds.
-}
oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    Decoder (\parsedFile -> oneOfHelp parsedFile decoders)


{-| A decoder that always succeeds with the result. May be useful in combination with `oneOf` to
provide a placeholder mesh if decoding fails.
-}
succeed : a -> Decoder a
succeed mesh =
    Decoder (\_ -> Result.Ok mesh)


{-| A decoder that always fails with a given error message.
Use it in case you need custom error messages.
-}
fail : String -> Decoder a
fail error =
    Decoder (\_ -> Result.Err error)



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


type alias ParsedFile =
    { positions : Array (Point3d Meters ObjCoordinates)
    , normals : Array (Vector3d Unitless ObjCoordinates)
    , uvs : Array ( Float, Float )
    , faces : List ( GroupProperties, List (List (List (Maybe Int))) )
    }


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


collectTexturedFaces : ParsedFile -> List ( GroupProperties, List (List (List (Maybe Int))) ) -> List ( TexturedFace, TexturedFace, TexturedFace ) -> Result String (TriangularMesh TexturedFace)
collectTexturedFaces parsedFile groups polygons =
    case groups of
        ( _, groupIndices ) :: remainingGroups ->
            case groupIndices of
                (first :: indices) :: remainingGroupIndices ->
                    case addTexturedFacesPolygons parsedFile first indices remainingGroupIndices polygons of
                        Ok newPolygons ->
                            collectTexturedFaces parsedFile remainingGroups newPolygons

                        Err error ->
                            Err error

                _ ->
                    Err "Empty number of indices in a group"

        [] ->
            Ok (TriangularMesh.triangles polygons)


addTexturedFacesPolygons : ParsedFile -> List (Maybe Int) -> List (List (Maybe Int)) -> List (List (List (Maybe Int))) -> List ( TexturedFace, TexturedFace, TexturedFace ) -> Result String (List ( TexturedFace, TexturedFace, TexturedFace ))
addTexturedFacesPolygons parsedFile firstVertex vertices groupIndices polygons =
    case vertices of
        _ :: [] ->
            case groupIndices of
                [] ->
                    Ok polygons

                (first :: indices) :: remainingGroupIndices ->
                    addTexturedFacesPolygons parsedFile first indices remainingGroupIndices polygons

                _ ->
                    Err "Missing indices"

        [ Just p2, Just uv2, Just n2 ] :: remainingVertices ->
            case remainingVertices of
                [ Just p3, Just uv3, Just n3 ] :: _ ->
                    case firstVertex of
                        [ Just p1, Just uv1, Just n1 ] ->
                            let
                                maybeV1 =
                                    Maybe.map3 TexturedFace (Array.get (p1 - 1) parsedFile.positions) (Array.get (n1 - 1) parsedFile.normals) (Array.get (uv1 - 1) parsedFile.uvs)

                                maybeV2 =
                                    Maybe.map3 TexturedFace (Array.get (p2 - 1) parsedFile.positions) (Array.get (n2 - 1) parsedFile.normals) (Array.get (uv2 - 1) parsedFile.uvs)

                                maybeV3 =
                                    Maybe.map3 TexturedFace (Array.get (p3 - 1) parsedFile.positions) (Array.get (n3 - 1) parsedFile.normals) (Array.get (uv3 - 1) parsedFile.uvs)
                            in
                            case Maybe.map3 (\v1 v2 v3 -> ( v1, v2, v3 )) maybeV1 maybeV2 maybeV3 of
                                Just triangle ->
                                    addTexturedFacesPolygons
                                        parsedFile
                                        firstVertex
                                        remainingVertices
                                        groupIndices
                                        (triangle :: polygons)

                                Nothing ->
                                    Err "Missing indices"

                        _ ->
                            Err "Missing indices"

                _ ->
                    Err "Missing indices"

        _ ->
            Err "Missing indices"


oneOfHelp : ParsedFile -> List (Decoder a) -> Result String a
oneOfHelp parsedFile decoders =
    case decoders of
        [] ->
            Err "Failed oneOf"

        (Decoder decoder) :: remainingDecoders ->
            case decoder parsedFile of
                Ok res ->
                    Ok res

                Err _ ->
                    oneOfHelp parsedFile remainingDecoders


parse : (Float -> Length) -> String -> Result String ParsedFile
parse units content =
    parseHelp units (String.lines content) [] [] [] [] Nothing Nothing [ "default" ] []


parseHelp :
    (Float -> Length)
    -> List String
    -> List (Point3d Meters ObjCoordinates)
    -> List (Vector3d Unitless ObjCoordinates)
    -> List ( Float, Float )
    -> List ( GroupProperties, List (List (List (Maybe Int))) )
    -> Maybe String
    -> Maybe String
    -> List String
    -> List (List (List (Maybe Int)))
    -> Result String ParsedFile
parseHelp units lines positions normals uvs faces_ object_ material_ groups indices =
    case lines of
        [] ->
            Result.Ok
                { positions = Array.fromList (List.reverse positions)
                , normals = Array.fromList (List.reverse normals)
                , uvs = Array.fromList (List.reverse uvs)
                , faces =
                    if indices == [] then
                        faces_

                    else
                        -- flush the last group of face indices
                        ( { groups = groups, object = object_, material = material_ }, indices ) :: faces_
                }

        line :: remainingLines ->
            case parseLine units line of
                Property propertyType ->
                    let
                        newFaces =
                            if indices == [] then
                                faces_

                            else
                                ( { groups = groups, object = object_, material = material_ }, indices ) :: faces_
                    in
                    case propertyType of
                        GroupsProperty newGroups ->
                            parseHelp units remainingLines positions normals uvs newFaces object_ material_ newGroups []

                        ObjectProperty newObject ->
                            parseHelp units remainingLines positions normals uvs newFaces (Just newObject) material_ groups []

                        MaterialProperty newMaterial ->
                            parseHelp units remainingLines positions normals uvs newFaces object_ (Just newMaterial) groups []

                PositionData position ->
                    parseHelp units remainingLines (position :: positions) normals uvs faces_ object_ material_ groups indices

                NormalData normal ->
                    parseHelp units remainingLines positions (normal :: normals) uvs faces_ object_ material_ groups indices

                UvData uv ->
                    parseHelp units remainingLines positions normals (uv :: uvs) faces_ object_ material_ groups indices

                FaceIndices faceIndices ->
                    parseHelp units remainingLines positions normals uvs faces_ object_ material_ groups (faceIndices :: indices)

                Skip ->
                    parseHelp units remainingLines positions normals uvs faces_ object_ material_ groups indices

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

            groups ->
                Property (GroupsProperty groups)

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
