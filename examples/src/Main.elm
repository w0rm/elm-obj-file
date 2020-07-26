module Main exposing (main)

import Angle
import Axis3d
import Browser
import Camera3d
import Color exposing (Color)
import Direction3d
import Html exposing (Html)
import Length
import Meshes.Pod
import Obj.Decode exposing (ObjCoordinates)
import Pixels
import Point3d
import Scene3d
import Scene3d.Material exposing (Texture)
import Scene3d.Mesh exposing (Textured)
import Task
import TriangularMesh
import Viewpoint3d
import WebGL.Texture


mesh : Textured ObjCoordinates
mesh =
    Meshes.Pod.obj
        |> Obj.Decode.decodeString Length.meters Obj.Decode.texturedFaces
        |> Result.withDefault TriangularMesh.empty
        |> Scene3d.Mesh.texturedFaces


type alias Model =
    Maybe (Texture Color)


type Msg
    = LoadedTexture (Result WebGL.Texture.Error (Texture Color))


init : () -> ( Model, Cmd Msg )
init () =
    ( Nothing
    , Meshes.Pod.texture
        |> Scene3d.Material.loadWith Scene3d.Material.nearestNeighborFiltering
        |> Task.attempt LoadedTexture
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update (LoadedTexture result) _ =
    ( Result.toMaybe result, Cmd.none )


view : Model -> Html Msg
view model =
    let
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.meters 0 0 0
                        , azimuth = Angle.degrees -45
                        , elevation = Angle.degrees 35
                        , distance = Length.meters 16
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    case model of
        Nothing ->
            Html.text "Loading…"

        Just texture ->
            Scene3d.sunny
                { upDirection = Direction3d.z
                , sunlightDirection = Direction3d.negativeZ
                , shadows = False
                , camera = camera
                , dimensions = ( Pixels.int 640, Pixels.int 640 )
                , background = Scene3d.transparentBackground
                , clipDepth = Length.meters 0.1
                , entities =
                    [ Scene3d.rotateAround Axis3d.x
                        (Angle.degrees 90)
                        (Scene3d.mesh
                            (Scene3d.Material.texturedNonmetal
                                { baseColor = texture
                                , roughness = Scene3d.Material.constant 1
                                }
                            )
                            mesh
                        )
                    ]
                }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
