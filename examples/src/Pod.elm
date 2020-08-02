module Pod exposing (main)

{-| This example demonstrates how to extract multiple meshes with
shadows from an OBJ file and render with elm-3d-scene.

The “Pod” model is courtesy of Kolja Wilcke <https://twitter.com/01k>

-}

import Angle
import Browser
import Camera3d
import Color exposing (Color)
import Direction3d
import Html exposing (Html)
import Http
import Length
import Obj.Decode exposing (Decoder, ObjCoordinates)
import Pixels
import Point3d
import Scene3d
import Scene3d.Material exposing (Texture)
import Scene3d.Mesh exposing (Shadow, Textured)
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
    }


type Msg
    = LoadedTexture (Result WebGL.Texture.Error (Texture Color))
    | LoadedMeshes (Result Http.Error Meshes)


init : () -> ( Model, Cmd Msg )
init () =
    ( { material = Nothing, meshes = Nothing }
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


view : Model -> Html Msg
view model =
    let
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.meters -0.5 0.5 0
                        , azimuth = Angle.degrees -45
                        , elevation = Angle.degrees 35
                        , distance = Length.meters 16
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    case ( model.material, model.meshes ) of
        ( Just material, Just { pod, guns, wheels } ) ->
            Scene3d.sunny
                { upDirection = Direction3d.z
                , sunlightDirection = Direction3d.negativeZ
                , shadows = True
                , camera = camera
                , dimensions = ( Pixels.int 640, Pixels.int 640 )
                , background = Scene3d.transparentBackground
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
                    , Scene3d.quad (Scene3d.Material.matte Color.blue)
                        (Point3d.meters -5 5 0)
                        (Point3d.meters 5 5 0)
                        (Point3d.meters 5 -5 0)
                        (Point3d.meters -5 -5 0)
                    ]
                }

        _ ->
            Html.text "Loading texture and meshes…"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
