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
    , faces : List ( GroupProperties, List String ) -- f 72/1/1 49/2/1 8/3/1 19/4/1
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
    fail "Implement texturedFaces"


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
    | FaceIndices String
    | Skip


parse : (Float -> Length) -> String -> Result String ParsedFile
parse units content =
    parseHelp units (String.lines content) [] [] [] [] Nothing Nothing [] []


parseHelp :
    (Float -> Length)
    -> List String
    -> List (Point3d Meters ObjCoordinates)
    -> List (Vector3d Unitless ObjCoordinates)
    -> List ( Float, Float )
    -> List ( GroupProperties, List String )
    -> Maybe String
    -> Maybe String
    -> List String
    -> List String
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
                Ok (Property propertyType) ->
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

                Ok (PositionData position) ->
                    parseHelp units remainingLines (position :: positions) normals uvs faces_ object_ material_ groups indices

                Ok (NormalData normal) ->
                    parseHelp units remainingLines positions (normal :: normals) uvs faces_ object_ material_ groups indices

                Ok (UvData uv) ->
                    parseHelp units remainingLines positions normals (uv :: uvs) faces_ object_ material_ groups indices

                Ok (FaceIndices faceIndices) ->
                    parseHelp units remainingLines positions normals uvs faces_ object_ material_ groups (faceIndices :: indices)

                Ok Skip ->
                    parseHelp units remainingLines positions normals uvs faces_ object_ material_ groups indices

                Err error ->
                    Err error


parseLine : (Float -> Length) -> String -> Result String Line
parseLine units line =
    if String.startsWith "o " line then
        case String.trim (String.dropLeft 2 line) of
            "" ->
                Result.Err "Expect object name"

            object_ ->
                Result.Ok (Property (ObjectProperty object_))

    else if String.startsWith "g " line then
        case String.words (String.dropLeft 2 line) of
            [] ->
                Result.Err "Expect group name"

            groups ->
                Result.Ok (Property (GroupsProperty groups))

    else if String.startsWith "usemtl " line then
        case String.trim (String.dropLeft 7 line) of
            "" ->
                Result.Err "Expect material name"

            material_ ->
                Result.Ok (Property (MaterialProperty material_))

    else if String.startsWith "v " line then
        case List.map String.toFloat (String.words (String.dropLeft 2 line)) of
            [ Just x, Just y, Just z ] ->
                Result.Ok (PositionData (Point3d.xyz (units x) (units y) (units z)))

            _ ->
                Err "Invalid position data"

    else if String.startsWith "vt " line then
        case List.map String.toFloat (String.words (String.dropLeft 3 line)) of
            [ Just x, Just y ] ->
                Result.Ok (UvData ( x, y ))

            _ ->
                Err "Invalid uv data"

    else if String.startsWith "vn " line then
        case List.map String.toFloat (String.words (String.dropLeft 3 line)) of
            [ Just x, Just y, Just z ] ->
                Result.Ok (NormalData (Vector3d.unitless x y z))

            _ ->
                Err "Invalid normal data"

    else if String.startsWith "f " line then
        case String.dropLeft 2 line of
            "" ->
                Result.Err "Expect face indices"

            faceIndices ->
                Result.Ok (FaceIndices faceIndices)

    else
        Ok Skip



{-
   { nextIndex : Int
   , mapping : Array Int
   , vertices : List (Point3d Meters ObjCoordinates) -- Array.fromList (List.reverse)
   , faceIndices : List (Int, Int, Int)
   }

   filterFile : (ParsedFile -> String -> Result String a) -> ParsedFile -> List Filter -> Result String (List a)

   let
       decoder =
           Obj.map2 Tuple.pair
               (Obj.faces [Obj.group "Gun", Obj.material "Plastic"])
               (Obj.triangles [Obj.group "Wheel"])
   in
   Obj.decode Length.centimeters decoder objFileContents

   Obj.decode Length.centimeters (Obj.faces []) objFileContents
-}
