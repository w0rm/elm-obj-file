module Pod exposing (main)

{-| This example demonstrates how to extract multiple meshes with
shadows from an OBJ file and render with elm-3d-scene.

The “Pod” model is courtesy of Kolja Wilcke <https://twitter.com/01k>

-}

import Angle exposing (Angle)
import Browser
import Browser.Events
import Camera3d
import Color exposing (Color)
import Direction3d
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Length
import Obj.Decode exposing (Decoder, ObjCoordinates)
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Material exposing (Texture)
import Scene3d.Mesh exposing (Shadow, Textured)
import SketchPlane3d
import Task
import Viewpoint3d
import WebGL.Texture


{-| Custom filter for all objects that start with a prefix.
-}
objectStartsWith : String -> Decoder a -> Decoder a
objectStartsWith prefix =
    Obj.Decode.filter
        (\{ object } ->
            case object of
                Just objectName ->
                    String.startsWith prefix objectName

                Nothing ->
                    False
        )


{-| Decode a list of meshes for matching object names.
-}
listOfObjects : Decoder a -> Decoder (List a)
listOfObjects decoder =
    Obj.Decode.objectNames
        |> Obj.Decode.andThen
            (\objectNames ->
                objectNames
                    |> List.map (\objectName -> Obj.Decode.object objectName decoder)
                    |> Obj.Decode.combine
            )


type alias MeshWithShadow =
    { mesh : Textured ObjCoordinates
    , shadow : Shadow ObjCoordinates
    }


{-| Decode a mesh together with the shadow.
-}
meshWithShadow : Decoder MeshWithShadow
meshWithShadow =
    Obj.Decode.map
        (\texturedFaces ->
            let
                mesh =
                    Scene3d.Mesh.texturedFaces texturedFaces
                        |> Scene3d.Mesh.cullBackFaces
            in
            MeshWithShadow mesh (Scene3d.Mesh.shadow mesh)
        )
        Obj.Decode.texturedFaces


type alias Meshes =
    { pod : MeshWithShadow
    , guns : List MeshWithShadow
    , wheels : List MeshWithShadow
    }


{-| Maps three decoders to get a decoder of the required meshes.
-}
meshes : Decoder Meshes
meshes =
    Obj.Decode.map3 Meshes
        (objectStartsWith "pod_" meshWithShadow)
        (objectStartsWith "gun_" (listOfObjects meshWithShadow))
        (objectStartsWith "wheel_" (listOfObjects meshWithShadow))


type alias Model =
    { material : Maybe (Scene3d.Material.Textured ObjCoordinates)
    , meshes : Maybe Meshes
    , azimuth : Angle
    , elevation : Angle
    , zoom : Float
    , orbiting : Bool
    }


type Msg
    = LoadedTexture (Result WebGL.Texture.Error (Texture Color))
    | LoadedMeshes (Result Http.Error Meshes)
    | MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseWheel Float


init : () -> ( Model, Cmd Msg )
init () =
    ( { material = Nothing
      , meshes = Nothing
      , azimuth = Angle.degrees -45
      , elevation = Angle.degrees 35
      , orbiting = False
      , zoom = 0
      }
    , Cmd.batch
        [ Scene3d.Material.loadWith Scene3d.Material.nearestNeighborFiltering "Pod.png"
            |> Task.attempt LoadedTexture
        , Http.get
            { url = "Pod.obj.txt" -- .txt is required to work with `elm reactor`
            , expect = Obj.Decode.expectObj LoadedMeshes Length.meters meshes
            }
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadedTexture result ->
            ( { model
                | material =
                    result
                        |> Result.map Scene3d.Material.texturedMatte
                        |> Result.toMaybe
              }
            , Cmd.none
            )

        LoadedMeshes result ->
            ( { model | meshes = Result.toMaybe result }
            , Cmd.none
            )

        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        MouseMove dx dy ->
            if model.orbiting then
                let
                    rotationRate =
                        Quantity.per Pixels.pixel (Angle.degrees 1)
                in
                ( { model
                    | azimuth =
                        model.azimuth
                            |> Quantity.minus (Quantity.at rotationRate dx)
                    , elevation =
                        model.elevation
                            |> Quantity.plus (Quantity.at rotationRate dy)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        MouseWheel deltaY ->
            ( { model | zoom = clamp 0 1 (model.zoom - deltaY * 0.002) }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.meters 0 0 1
                        , azimuth = model.azimuth
                        , elevation = model.elevation
                        , distance = Length.meters (16 - model.zoom * 8)
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    case ( model.material, model.meshes ) of
        ( Just material, Just { pod, guns, wheels } ) ->
            Html.figure
                [ Html.Attributes.style "display" "block"
                , Html.Attributes.style "width" "640px"
                , Html.Attributes.style "margin" "auto"
                , Html.Attributes.style "padding" "20px"
                , Html.Events.preventDefaultOn "wheel"
                    (Json.Decode.map
                        (\deltaY -> ( MouseWheel deltaY, True ))
                        (Json.Decode.field "deltaY" Json.Decode.float)
                    )
                ]
                [ Scene3d.sunny
                    { upDirection = Direction3d.z
                    , sunlightDirection =
                        Direction3d.fromAzimuthInAndElevationFrom SketchPlane3d.xy
                            (Angle.degrees 135)
                            (Angle.degrees -55)
                    , shadows = True
                    , camera = camera
                    , dimensions = ( Pixels.int 640, Pixels.int 640 )
                    , background = Scene3d.backgroundColor Color.lightGray
                    , clipDepth = Length.meters 0.1
                    , entities =
                        [ Scene3d.meshWithShadow material pod.mesh pod.shadow
                        , case List.head (List.drop 2 guns) of
                            Just { mesh, shadow } ->
                                Scene3d.meshWithShadow material mesh shadow

                            Nothing ->
                                Scene3d.nothing
                        , wheels
                            |> List.map
                                (\{ mesh, shadow } ->
                                    Scene3d.meshWithShadow material mesh shadow
                                )
                            |> Scene3d.group
                        , Scene3d.quad (Scene3d.Material.matte Color.lightBlue)
                            (Point3d.meters -5 5 -0.02)
                            (Point3d.meters 5 5 -0.02)
                            (Point3d.meters 5 -5 -0.02)
                            (Point3d.meters -5 -5 -0.02)
                        ]
                    }
                , Html.figcaption [ Html.Attributes.style "font" "14px/1.5 sans-serif" ]
                    [ Html.p []
                        [ Html.text "The “Pod” model is courtesy of "
                        , Html.a
                            [ Html.Attributes.href "https://twitter.com/01k"
                            , Html.Attributes.target "_blank"
                            ]
                            [ Html.text "Kolja Wilcke"
                            ]
                        ]
                    ]
                ]

        _ ->
            Html.text "Loading texture and meshes…"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.orbiting then
        Sub.batch
            [ Browser.Events.onMouseMove decodeMouseMove
            , Browser.Events.onMouseUp (Json.Decode.succeed MouseUp)
            ]

    else
        Browser.Events.onMouseDown (Json.Decode.succeed MouseDown)


decodeMouseMove : Json.Decode.Decoder Msg
decodeMouseMove =
    Json.Decode.map2 MouseMove
        (Json.Decode.field "movementX" (Json.Decode.map Pixels.float Json.Decode.float))
        (Json.Decode.field "movementY" (Json.Decode.map Pixels.float Json.Decode.float))
