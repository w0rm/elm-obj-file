module Viewer exposing (main)

{-| This example demonstrates how to load a mesh from file.
It can also be used to test the parser :-)

Check <http://people.math.sc.edu/Burkardt/data/obj/obj.html> for sample obj files

-}

import Angle
import Array
import BoundingBox3d exposing (BoundingBox3d)
import Browser
import Camera3d
import Color exposing (Color)
import Direction3d
import File exposing (File)
import File.Select
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Length exposing (Meters)
import Obj.Decode exposing (Decoder, ObjCoordinates)
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Material exposing (Texture)
import Scene3d.Mesh exposing (Plain, Textured, Uniform, Unlit)
import Task
import TriangularMesh
import Viewpoint3d
import WebGL.Texture exposing (Error(..))


type ViewMesh
    = ViewTextured (Textured ObjCoordinates)
    | ViewUniform (Uniform ObjCoordinates)


{-| Because we don’t know the exect format of mesh, we try decoding different
primitives: from the most specific to the most simple one.
-}
viewMesh : Decoder ( BoundingBox3d Meters ObjCoordinates, ViewMesh )
viewMesh =
    let
        boundingBox position triangularMesh =
            case List.map position (Array.toList (TriangularMesh.vertices triangularMesh)) of
                first :: rest ->
                    BoundingBox3d.hull first rest

                [] ->
                    BoundingBox3d.hull Point3d.origin []
    in
    Obj.Decode.oneOf
        [ Obj.Decode.map (\triangularMesh -> ( boundingBox .position triangularMesh, ViewTextured (Scene3d.Mesh.texturedFaces triangularMesh) ))
            Obj.Decode.texturedFaces
        , Obj.Decode.map (\triangularMesh -> ( boundingBox .position triangularMesh, ViewUniform (Scene3d.Mesh.indexedFaces triangularMesh) ))
            Obj.Decode.faces
        , Obj.Decode.map (\triangularMesh -> ( boundingBox .position triangularMesh, ViewTextured (Scene3d.Mesh.texturedFacets triangularMesh) ))
            Obj.Decode.texturedTriangles
        , Obj.Decode.map (\triangularMesh -> ( boundingBox identity triangularMesh, ViewUniform (Scene3d.Mesh.indexedFacets triangularMesh) ))
            Obj.Decode.triangles
        ]


type LoadState a
    = Empty
    | Loaded a
    | Error String


type alias Model =
    { texture : LoadState (Texture Color)
    , mesh : LoadState ( BoundingBox3d Meters ObjCoordinates, ViewMesh )
    , hover : Bool
    }


type Msg
    = PickClicked
    | ResetClicked
    | DragEnter
    | DragLeave
    | LoadedTexture (Result WebGL.Texture.Error (Texture Color))
    | LoadedMesh (Result String ( BoundingBox3d Meters ObjCoordinates, ViewMesh ))
    | GotFiles File (List File)


init : () -> ( Model, Cmd Msg )
init () =
    ( { mesh = Empty, texture = Empty, hover = False }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResetClicked ->
            ( { model | texture = Empty, mesh = Empty }, Cmd.none )

        PickClicked ->
            ( model, File.Select.files [] GotFiles )

        DragEnter ->
            ( { model | hover = True }, Cmd.none )

        DragLeave ->
            ( { model | hover = False }, Cmd.none )

        GotFiles file files ->
            let
                ( imageFiles, objFiles ) =
                    List.partition
                        (\f -> String.startsWith "image/" (File.mime f))
                        (file :: files)

                loadTexture =
                    case imageFiles of
                        textureFile :: _ ->
                            File.toUrl textureFile
                                |> Task.andThen
                                    (\url ->
                                        Scene3d.Material.loadWith Scene3d.Material.nearestNeighborFiltering url
                                    )
                                |> Task.attempt LoadedTexture

                        _ ->
                            Cmd.none

                loadMesh =
                    case objFiles of
                        objFile :: _ ->
                            File.toString objFile
                                |> Task.andThen
                                    (\string ->
                                        case
                                            Obj.Decode.decodeString
                                                Length.meters
                                                viewMesh
                                                string
                                        of
                                            Ok m ->
                                                Task.succeed m

                                            Err err ->
                                                Task.fail err
                                    )
                                |> Task.attempt LoadedMesh

                        _ ->
                            Cmd.none
            in
            ( { model | hover = False }, Cmd.batch [ loadTexture, loadMesh ] )

        LoadedMesh result ->
            ( { model
                | mesh =
                    case result of
                        Err err ->
                            Error err

                        Ok m ->
                            Loaded m
              }
            , Cmd.none
            )

        LoadedTexture result ->
            ( { model
                | texture =
                    case result of
                        Err LoadError ->
                            Error "Texture load error"

                        Err (SizeError _ _) ->
                            Error "Texture size error"

                        Ok texture ->
                            Loaded texture
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        maybeMesh =
            case model.mesh of
                Loaded m ->
                    Just m

                _ ->
                    Nothing

        maybeMeshError =
            case model.mesh of
                Error m ->
                    Just m

                _ ->
                    Nothing
    in
    Html.div
        [ Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "justify-content" "center"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100%"
        , hijackOn "dragenter" (Json.Decode.succeed DragEnter)
        , hijackOn "dragover" (Json.Decode.succeed DragEnter)
        , hijackOn "dragleave" (Json.Decode.succeed DragLeave)
        , hijackOn "drop" dropDecoder
        ]
        [ Html.div
            [ Html.Attributes.style "border"
                (if model.hover then
                    "3px dashed green"

                 else
                    case model.mesh of
                        Loaded _ ->
                            "3px dashed rgb(52, 101, 164)"

                        _ ->
                            "3px dashed #ccc"
                )
            , Html.Attributes.style "width" "640px"
            , Html.Attributes.style "height" "640px"
            , Html.Attributes.style "margin" "0 0 10px"
            , Html.Attributes.style "align-items" "center"
            , Html.Attributes.style "justify-content" "center"
            , Html.Attributes.style "flex-direction" "column"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "position" "relative"
            ]
            (case maybeMesh of
                Nothing ->
                    [ case maybeMeshError of
                        Just err ->
                            Html.p [ Html.Attributes.style "color" "red" ]
                                [ Html.text err ]

                        Nothing ->
                            Html.text ""
                    , Html.button [ Html.Events.onClick PickClicked ]
                        [ Html.text "select an OBJ file and/or an image" ]
                    ]

                Just ( boundingBox, mesh ) ->
                    let
                        { minX, maxX, minY, maxY, minZ, maxZ } =
                            BoundingBox3d.extrema boundingBox

                        distance =
                            List.map Quantity.abs [ minX, maxX, minY, maxY, minZ, maxZ ]
                                |> List.foldl Quantity.max Quantity.zero
                                |> Quantity.multiplyBy 4

                        camera =
                            Camera3d.perspective
                                { viewpoint =
                                    Viewpoint3d.orbitZ
                                        { focalPoint = BoundingBox3d.centerPoint boundingBox
                                        , azimuth = Angle.degrees -45
                                        , elevation = Angle.degrees 35
                                        , distance = distance
                                        }
                                , verticalFieldOfView = Angle.degrees 30
                                }

                        entity =
                            case mesh of
                                ViewTextured texturedMesh ->
                                    case model.texture of
                                        Loaded texture ->
                                            Scene3d.mesh (Scene3d.Material.texturedMatte texture)
                                                texturedMesh

                                        _ ->
                                            Scene3d.mesh (Scene3d.Material.matte Color.blue)
                                                texturedMesh


                                ViewUniform uniformMesh ->
                                    Scene3d.mesh (Scene3d.Material.matte Color.blue)
                                        uniformMesh
                    in
                    [ Scene3d.sunny
                        { upDirection = Direction3d.z
                        , sunlightDirection = Direction3d.negativeZ
                        , shadows = True
                        , camera = camera
                        , dimensions = ( Pixels.int 640, Pixels.int 640 )
                        , background = Scene3d.transparentBackground
                        , clipDepth = Length.meters 0.1
                        , entities = [ entity ]
                        }
                    , Html.button
                        [ Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "right" "10px"
                        , Html.Attributes.style "top" "10px"
                        , Html.Events.onClick ResetClicked
                        ]
                        [ Html.text "close" ]
                    ]
            )
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


dropDecoder : Json.Decode.Decoder Msg
dropDecoder =
    Json.Decode.at [ "dataTransfer", "files" ]
        (Json.Decode.oneOrMore GotFiles File.decoder)


hijackOn : String -> Json.Decode.Decoder msg -> Attribute msg
hijackOn event decoder =
    Html.Events.preventDefaultOn event (Json.Decode.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )
