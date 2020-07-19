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
decodeString =
    Debug.todo "Implement decodeString"


triangles : List Filter -> Decoder (TriangularMesh (Point3d Meters ObjCoordinates))
triangles =
    Debug.todo "Implement triangles"


faces : List Filter -> Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, normal : Vector3d Unitless ObjCoordinates })
faces =
    Debug.todo "Implement faces"


texturedTriangles : List Filter -> Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, uv : ( Float, Float ) })
texturedTriangles =
    Debug.todo "Implement texturedTriangles"


texturedFaces : List Filter -> Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, normal : Vector3d Unitless ObjCoordinates, uv : ( Float, Float ) })
texturedFaces =
    Debug.todo "Implement texturedFaces"


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
