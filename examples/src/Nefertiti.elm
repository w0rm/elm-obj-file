module Nefertiti exposing (main)

{-| The following example demonstrates loading bumpy faces to render
elm-3d-scene bumpy materials.

You can use Blender to reduce the size of a mesh by baking details
into the normal map: <https://www.katsbits.com/codex/bake-normal-maps/>

The OBJ file was derived from the original “Bust of Nefertiti”
scan by Staatliche Museen zu Berlin – Preußischer Kulturbesitz,
under the CC BY-NC-SA license.

Toggle bumpy material to see if it makes the difference!

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
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import Obj.Decode exposing (ObjCoordinates)
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, Unitless)
import Scene3d
import Scene3d.Material exposing (Texture)
import Scene3d.Mesh exposing (Bumpy)
import SketchPlane3d
import Task
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import Viewpoint3d
import WebGL.Texture


type alias Model =
    { azimuth : Angle
    , elevation : Angle
    , zoom : Float
    , orbiting : Bool
    , colorTexture : Maybe (Texture Color)
    , normalMap : Maybe Scene3d.Material.NormalMap
    , mesh : Maybe (Bumpy ObjCoordinates)
    , useBumpyMaterial : Bool
    , useColorTexture : Bool
    }


type Msg
    = LoadedColorTexture (Result WebGL.Texture.Error (Texture Color))
    | LoadedNormalMap (Result WebGL.Texture.Error Scene3d.Material.NormalMap)
    | LoadedMesh
        (Result
            Http.Error
            (TriangularMesh
                { position : Point3d Meters ObjCoordinates
                , normal : Vector3d Unitless ObjCoordinates
                , uv : ( Float, Float )
                , tangent : Vector3d Unitless ObjCoordinates
                , tangentBasisIsRightHanded : Bool
                }
            )
        )
    | MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseWheel Float
    | UseBumpyMaterialToggled Bool
    | UseColorTextureToggled Bool


init : () -> ( Model, Cmd Msg )
init () =
    ( { colorTexture = Nothing
      , normalMap = Nothing
      , mesh = Nothing
      , azimuth = Angle.degrees -50
      , elevation = Angle.degrees 15
      , orbiting = False
      , useBumpyMaterial = False
      , useColorTexture = True
      , zoom = 0
      }
    , Cmd.batch
        [ Task.attempt LoadedColorTexture (Scene3d.Material.load "NefertitiColor.png")
        , Task.attempt LoadedNormalMap (Scene3d.Material.loadNormalMap "NefertitiNormalMap.png")
        , Http.get
            { url = "Nefertiti.obj.txt" -- .txt is required to work with `elm reactor`
            , expect =
                Obj.Decode.expectObj LoadedMesh
                    Length.meters
                    Obj.Decode.bumpyFaces
            }
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadedColorTexture result ->
            ( { model | colorTexture = Result.toMaybe result }
            , Cmd.none
            )

        LoadedNormalMap result ->
            ( { model | normalMap = Result.toMaybe result }
            , Cmd.none
            )

        LoadedMesh result ->
            ( { model
                | mesh =
                    result
                        |> Result.map Scene3d.Mesh.bumpyFaces
                        |> Result.map Scene3d.Mesh.cullBackFaces
                        |> Result.toMaybe
              }
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

        UseBumpyMaterialToggled bumpy ->
            ( { model | useBumpyMaterial = bumpy }, Cmd.none )

        UseColorTextureToggled color ->
            ( { model | useColorTexture = color }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.meters 0 0 0.2
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters (1.2 - model.zoom * 0.5)
                }

        sunlightDirection =
            Direction3d.fromAzimuthInAndElevationFrom SketchPlane3d.xy
                model.azimuth
                model.elevation
                |> Direction3d.reverse

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    case ( model.colorTexture, model.normalMap, model.mesh ) of
        ( Just colorTexture, Just normalMapTexture, Just mesh ) ->
            let
                material =
                    case ( model.useBumpyMaterial, model.useColorTexture ) of
                        ( True, True ) ->
                            Scene3d.Material.bumpyNonmetal
                                { baseColor = colorTexture
                                , roughness = Scene3d.Material.constant 0.5
                                , ambientOcclusion = Scene3d.Material.constant 1
                                , normalMap = normalMapTexture
                                }

                        ( False, True ) ->
                            Scene3d.Material.texturedNonmetal
                                { baseColor = colorTexture
                                , roughness = Scene3d.Material.constant 0.5
                                }

                        ( True, False ) ->
                            Scene3d.Material.bumpyNonmetal
                                { baseColor = Scene3d.Material.constant Color.blue
                                , roughness = Scene3d.Material.constant 0.5
                                , ambientOcclusion = Scene3d.Material.constant 1
                                , normalMap = normalMapTexture
                                }

                        ( False, False ) ->
                            Scene3d.Material.texturedNonmetal
                                { baseColor = Scene3d.Material.constant Color.blue
                                , roughness = Scene3d.Material.constant 0.5
                                }
            in
            Html.figure
                [ Html.Attributes.style "display" "block"
                , Html.Attributes.style "width" "640px"
                , Html.Attributes.style "margin" "auto"
                , Html.Attributes.style "padding" "20px"
                , Html.Events.preventDefaultOn "wheel"
                    (Decode.map
                        (\deltaY -> ( MouseWheel deltaY, True ))
                        (Decode.field "deltaY" Decode.float)
                    )
                ]
                [ Scene3d.sunny
                    { upDirection = Direction3d.z
                    , sunlightDirection = sunlightDirection
                    , shadows = True
                    , camera = camera
                    , dimensions = ( Pixels.int 640, Pixels.int 640 )
                    , background = Scene3d.backgroundColor Color.darkGrey
                    , clipDepth = Length.meters 0.01
                    , entities = [ Scene3d.mesh material mesh ]
                    }
                , Html.figcaption [ Html.Attributes.style "font" "14px/1.5 sans-serif" ]
                    [ Html.p []
                        [ Html.text "This is a simplified version of the "
                        , Html.a
                            [ Html.Attributes.href "https://www.thingiverse.com/thing:3974391"
                            , Html.Attributes.target "_blank"
                            ]
                            [ Html.text "Bust of Nefertiti"
                            ]
                        , Html.text " by Staatliche Museen zu Berlin – Preußischer Kulturbesitz, under the "
                        , Html.a
                            [ Html.Attributes.href "https://creativecommons.org/licenses/by-nc-sa/4.0/"
                            , Html.Attributes.target "_blank"
                            ]
                            [ Html.text "CC BY-NC-SA license" ]
                        ]
                    , Html.p []
                        [ Html.label []
                            [ Html.input
                                [ Html.Attributes.type_ "checkbox"
                                , Html.Attributes.checked model.useBumpyMaterial
                                , Html.Events.onCheck UseBumpyMaterialToggled
                                ]
                                []
                            , Html.text " Use bumpy material"
                            ]
                        , Html.label [ Html.Attributes.style "margin-left" "20px" ]
                            [ Html.input
                                [ Html.Attributes.type_ "checkbox"
                                , Html.Attributes.checked model.useColorTexture
                                , Html.Events.onCheck UseColorTextureToggled
                                ]
                                []
                            , Html.text " Use color texture"
                            ]
                        ]
                    ]
                ]

        _ ->
            Html.text "Loading mesh and textures…"


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
            , Browser.Events.onMouseUp (Decode.succeed MouseUp)
            ]

    else
        Browser.Events.onMouseDown (Decode.succeed MouseDown)


decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))
