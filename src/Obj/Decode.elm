module Obj.Decode exposing
    ( Decoder
    , Filter
    , ObjCoordinates
    , decodeString
    , defaultGroup
    , faces
    , fail
    , group
    , map2
    , material
    , object
    , oneOf
    , succeed
    , texturedFaces
    , texturedTriangles
    , triangles
    )

import Array exposing (Array)
import Length exposing (Length, Meters)
import Point3d exposing (Point3d)
import Quantity exposing (Unitless)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


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


type Decoder a
    = Decoder (ParsedFile -> Result String a)


decodeString : (Float -> Length) -> Decoder a -> String -> Result String a
decodeString units (Decoder decode) content =
    content
        |> parse units
        |> Result.andThen decode


triangles : List Filter -> Decoder (TriangularMesh (Point3d Meters ObjCoordinates))
triangles filters =
    fail "Implement triangles"


faces : List Filter -> Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, normal : Vector3d Unitless ObjCoordinates })
faces filters =
    fail "Implement faces"


texturedTriangles : List Filter -> Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, uv : ( Float, Float ) })
texturedTriangles filters =
    fail "Implement texturedTriangles"


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


type alias TexturedFace =
    { position : Point3d Meters ObjCoordinates
    , normal : Vector3d Unitless ObjCoordinates
    , uv : ( Float, Float )
    }


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


type Filter
    = Group String
    | Material String
    | Object String


group : String -> Filter
group =
    Group


defaultGroup : Filter
defaultGroup =
    Group "default"


object : String -> Filter
object =
    Object


material : String -> Filter
material =
    Material


map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 fn (Decoder decoderA) (Decoder decoderB) =
    Decoder (\parsedFile -> Result.map2 fn (decoderA parsedFile) (decoderB parsedFile))


oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    Decoder (\parsedFile -> oneOfHelp parsedFile decoders)


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


succeed : a -> Decoder a
succeed mesh =
    Decoder (\_ -> Result.Ok mesh)


fail : String -> Decoder a
fail error =
    Decoder (\_ -> Result.Err error)



-- Internals


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
