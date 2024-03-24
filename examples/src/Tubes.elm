module Tubes exposing (main)

{-| This example demonstrates how to save a triangular mesh
in OBJ format with `Obj.Encode.encode`.
-}

import Angle exposing (Angle)
import Array
import Axis3d
import BoundingBox3d exposing (BoundingBox3d)
import Browser
import Camera3d
import Color
import Direction3d
import File.Download
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Length exposing (Meters)
import Obj.Decode exposing (ObjCoordinates)
import Obj.Encode
import Path
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, Unitless)
import Scene3d
import Scene3d.Material
import Scene3d.Mesh exposing (Shadow, Uniform)
import SubPath
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import Viewpoint3d


type alias Model =
    { mesh : Maybe Mesh
    , azimuth : Angle
    , elevation : Angle
    , zoom : Float
    , orbiting : Bool
    , svgPath : String
    }


type alias Mesh =
    { mesh : Uniform ObjCoordinates
    , shadow : Shadow ObjCoordinates
    , boundingBox : BoundingBox3d Meters ObjCoordinates
    , triangularMesh :
        TriangularMesh
            { position : Point3d Meters ObjCoordinates
            , normal : Vector3d Unitless ObjCoordinates
            }
    }


type Msg
    = MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseWheel Float
    | TextareaChanged String
    | SaveClicked


init : () -> ( Model, Cmd Msg )
init () =
    let
        svgPath =
            "M738.905,301.344c-48.612,0-87.693,32.409-87.693,77.208c0,25.736,19.064,47.66,42.894,47.66c24.783,0,40.988-9.532,40.988-21.924c0-8.579-5.72-12.392-19.064-12.392c-28.596,0-50.519,24.783-50.519,57.191c0,28.596,21.923,50.52,55.285,50.52c38.127,0,64.815-15.251,78.161-34.314M396.709,593.021c-9.532,81.021-66.724,168.715-97.226,168.715c-10.485,0-20.97-20.97-20.97-53.378c0-74.35,36.222-144.886,58.145-144.886c22.876,0,63.864,101.039,63.864,186.826c0,2.859,0,9.531-0.953,13.345M345.237,328.034c-16.204,64.817-104.852,170.622-137.26,170.622c-7.625,0-11.438-6.673-11.438-24.782c0-83.881,41.94-174.436,67.677-174.436c40.034,0,62.911,138.214,60.051,194.452M479.637,743.627c-2.859-12.392-4.766-28.597-4.766-44.801c0-81.975,26.689-134.4,50.519-134.4c18.111,0,36.222,30.502,36.222,77.208c0,23.83-6.672,38.128-15.251,38.128c-6.673,0-12.392-8.578-12.392-30.502c0-35.268,23.83-66.724,46.707-66.724c23.829,0,44.8,38.129,44.8,112.478c0,29.549-5.719,55.285-13.345,67.677M112.657,715.03c0,27.643,18.111,46.706,40.988,46.706c29.548,0,47.66-30.501,47.66-88.646c0-37.175-2.86-73.396-8.579-108.664M421.492,497.703c-5.719-16.205-9.532-41.941-9.532-75.304c0-63.863,24.784-107.71,49.566-107.71c17.157,0,37.175,24.783,37.175,59.098c0,20.017-7.626,33.361-18.11,33.361c-9.532,0-16.205-10.485-16.205-32.408c0-43.847,25.736-75.303,51.473-75.303c31.456,0,61.957,49.567,61.957,129.635c0,21.923-3.812,52.426-8.578,68.631M134.581,311.83c-9.532-7.626-23.83-12.392-42.894-12.392c-48.613,0-89.6,40.987-89.6,106.759c0,60.05,27.643,92.459,64.817,92.459c47.659,0,81.021-28.596,81.021-62.91c0-20.971-9.532-29.549-42.893-29.549c-14.298,0-34.315,3.812-49.566,9.531M444.369,188.868c-0.954-3.813-1.906-11.438-1.906-18.111c0-57.192,34.314-134.4,58.145-134.4c17.157,0,39.08,40.987,39.08,90.553c0,24.783-5.719,35.268-13.344,35.268c-6.673,0-10.485-4.766-10.485-16.204c0-47.66,40.034-109.617,65.771-109.617c22.876,0,45.754,40.987,45.754,122.009c0,20.017-2.86,47.66-6.673,68.63M340.471,36.356c-9.532,51.473-17.157,116.29-17.157,155.371c0,31.455,8.579,42.894,28.596,42.894c26.689,0,75.303-9.532,91.506-16.205M227.994,36.356c-48.613,0-87.694,32.409-87.694,77.208c0,25.736,19.064,47.66,42.893,47.66c24.783,0,40.988-9.532,40.988-21.924c0-8.579-5.719-12.392-19.064-12.392c-28.596,0-50.519,24.783-50.519,57.192c0,28.596,21.923,50.52,55.285,50.52c38.127,0,64.817-15.251,78.162-34.315"
    in
    ( { mesh = tubes svgPath
      , azimuth = Angle.degrees -45
      , elevation = Angle.degrees 15
      , orbiting = False
      , zoom = 0
      , svgPath = svgPath
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        TextareaChanged text ->
            ( { model
                | svgPath = text
                , mesh = tubes text
              }
            , Cmd.none
            )

        SaveClicked ->
            ( model
            , case model.mesh of
                Just { triangularMesh } ->
                    File.Download.string "Tubes.obj"
                        "model/obj"
                        (Obj.Encode.encode Length.inMeters (Obj.Encode.faces triangularMesh))

                Nothing ->
                    Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.figure
        [ Html.Attributes.style "display" "block"
        , Html.Attributes.style "width" "640px"
        , Html.Attributes.style "margin" "auto"
        , Html.Attributes.style "padding" "20px"
        , Html.Attributes.style "font" "14px/1.5 sans-serif"
        ]
        [ Html.div
            ([ Html.Attributes.style "width" "640px"
             , Html.Attributes.style "height" "640px"
             , Html.Attributes.style "position" "relative"
             , Html.Attributes.style "background" (Color.toCssString Color.lightGray)
             ]
                ++ mouseEvents model.orbiting
            )
            (case model.mesh of
                Just { mesh, shadow, boundingBox } ->
                    let
                        focalPoint =
                            BoundingBox3d.centerPoint boundingBox

                        ( x, y, _ ) =
                            BoundingBox3d.dimensions boundingBox

                        distance =
                            Quantity.max x y

                        camera =
                            Camera3d.perspective
                                { viewpoint =
                                    Viewpoint3d.orbitZ
                                        { focalPoint = focalPoint
                                        , azimuth = model.azimuth
                                        , elevation = model.elevation
                                        , distance = Quantity.multiplyBy (3 - 2 * model.zoom) distance
                                        }
                                , verticalFieldOfView = Angle.degrees 30
                                }
                    in
                    [ Scene3d.sunny
                        { upDirection = Direction3d.z
                        , sunlightDirection = Direction3d.y
                        , shadows = True
                        , camera = camera
                        , dimensions = ( Pixels.int 640, Pixels.int 640 )
                        , background = Scene3d.transparentBackground
                        , clipDepth = Length.meters 0.1
                        , entities =
                            [ Scene3d.meshWithShadow (Scene3d.Material.matte Color.red) mesh shadow
                                |> Scene3d.rotateAround (Axis3d.through focalPoint Direction3d.x) (Angle.degrees 90)
                            , Scene3d.quad (Scene3d.Material.matte Color.lightGray)
                                (Point3d.xyz (Quantity.negate distance) (Length.meters 0.2) distance)
                                (Point3d.xyz distance (Length.meters 0.2) distance)
                                (Point3d.xyz distance (Length.meters 0.2) (Quantity.negate distance))
                                (Point3d.xyz (Quantity.negate distance) (Length.meters 0.2) (Quantity.negate distance))
                                |> Scene3d.translateBy (Vector3d.from Point3d.origin focalPoint)
                            ]
                        }
                    , Html.button
                        [ Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "right" "10px"
                        , Html.Attributes.style "bottom" "10px"
                        , Html.Events.onClick SaveClicked
                        ]
                        [ Html.text "Save in OBJ format" ]
                    ]

                _ ->
                    []
            )
        , Html.figcaption []
            [ Html.p []
                [ Html.text "The “elm game jam” lettering is courtesy of "
                , Html.a
                    [ Html.Attributes.href "https://github.com/kuzminadya"
                    , Html.Attributes.target "_blank"
                    ]
                    [ Html.text "Nadya Kuzmina"
                    ]
                ]
            ]
        , Html.textarea
            [ Html.Events.onInput TextareaChanged
            , Html.Attributes.rows 5
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.style "box-sizing" "border-box"
            , Html.Attributes.style "font-family" "monospace"
            , Html.Attributes.style "word-break" "break-all"
            , Html.Attributes.style "border-color"
                (if model.mesh == Nothing then
                    "red"

                 else
                    "default"
                )
            ]
            [ Html.text model.svgPath ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


mouseEvents : Bool -> List (Html.Attribute Msg)
mouseEvents orbiting =
    let
        mouseWheelEvent =
            Html.Events.preventDefaultOn "wheel"
                (Json.Decode.map
                    (\deltaY -> ( MouseWheel deltaY, True ))
                    (Json.Decode.field "deltaY" Json.Decode.float)
                )
    in
    if orbiting then
        [ Html.Events.on "mousemove" decodeMouseMove
        , Html.Events.onMouseUp MouseUp
        , mouseWheelEvent
        ]

    else
        [ Html.Events.onMouseDown MouseDown
        , mouseWheelEvent
        ]


decodeMouseMove : Json.Decode.Decoder Msg
decodeMouseMove =
    Json.Decode.map2 MouseMove
        (Json.Decode.field "movementX" (Json.Decode.map Pixels.float Json.Decode.float))
        (Json.Decode.field "movementY" (Json.Decode.map Pixels.float Json.Decode.float))


{-| Generate tubes mesh from SVG path
-}
tubes : String -> Maybe Mesh
tubes svgPath =
    let
        radius =
            Length.meters 0.01

        segment =
            5

        slope =
            0

        resolution =
            8

        pathToEntity path result =
            path
                |> SubPath.arcLengthParameterized 0.01
                |> (\parametrization ->
                        let
                            length =
                                SubPath.arcLength parametrization

                            subdivisions =
                                round (length / segment)

                            pointOnPath u =
                                let
                                    ( px, py ) =
                                        SubPath.pointAlong parametrization (length * u)
                                            |> Maybe.withDefault ( 0, 0 )

                                    ( tx, ty ) =
                                        SubPath.tangentAlong parametrization (length * u)
                                            |> Maybe.withDefault ( 0, 0 )
                                in
                                { position = Point3d.millimeters px -py (u * slope)
                                , tangent = Direction3d.unsafe { x = tx, y = -ty, z = 0 }
                                , normal = Direction3d.z
                                }

                            endCap p =
                                let
                                    { tangent, position, normal } =
                                        pointOnPath p

                                    flippedNormal =
                                        if p == 0 then
                                            Direction3d.toVector (Direction3d.reverse tangent)

                                        else
                                            Direction3d.toVector tangent
                                in
                                TriangularMesh.radial
                                    { position = position
                                    , normal = flippedNormal
                                    }
                                    (List.map
                                        (\i ->
                                            let
                                                u =
                                                    if p == 0 then
                                                        -(toFloat i / toFloat resolution)

                                                    else
                                                        toFloat i / toFloat resolution

                                                frame =
                                                    Frame3d.atOrigin
                                                        |> Frame3d.translateIn normal radius
                                                        |> Frame3d.rotateAround (Axis3d.withDirection tangent position) (Angle.turns u)
                                            in
                                            { position = Point3d.placeIn frame position
                                            , normal = flippedNormal
                                            }
                                        )
                                        (List.range 0 resolution)
                                    )

                            tube =
                                TriangularMesh.tube subdivisions
                                    resolution
                                    (\u v ->
                                        let
                                            { tangent, position, normal } =
                                                pointOnPath u

                                            frame =
                                                Frame3d.atOrigin
                                                    |> Frame3d.translateIn normal radius
                                                    |> Frame3d.rotateAround (Axis3d.withDirection tangent position) (Angle.turns -v)
                                        in
                                        { position = Point3d.placeIn frame position
                                        , normal = Direction3d.toVector (Direction3d.placeIn frame normal)
                                        }
                                    )
                        in
                        tube :: endCap 0 :: endCap 1 :: result
                   )
    in
    svgPath
        |> Path.parse
        |> Result.toMaybe
        |> Maybe.map
            (\result ->
                let
                    triangularMesh =
                        TriangularMesh.combine (List.foldl pathToEntity [] result)

                    boundingBox =
                        triangularMesh
                            |> TriangularMesh.vertices
                            |> Array.toList
                            |> List.map .position
                            |> BoundingBox3d.hullN
                            |> Maybe.withDefault (BoundingBox3d.singleton Point3d.origin)

                    mesh =
                        Scene3d.Mesh.indexedFaces triangularMesh
                in
                { mesh = Scene3d.Mesh.cullBackFaces mesh
                , shadow = Scene3d.Mesh.shadow mesh
                , boundingBox = boundingBox
                , triangularMesh = triangularMesh
                }
            )
