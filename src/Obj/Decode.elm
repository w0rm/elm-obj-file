module Obj.Decode exposing
    ( Decoder
    , triangles, faces, texturedTriangles, texturedFaces, polylines
    , decodeString, expectObj
    , object, group, defaultGroup, material
    , objectNames, groupNames, materialNames
    , map, map2, map3, map4, map5
    , filter, oneOf, fail, succeed, andThen, combine
    , ObjCoordinates
    , trianglesIn, facesIn, texturedTrianglesIn, texturedFacesIn, polylinesIn
    )

{-|

@docs Decoder


# Primitives

elm-obj-file currently supports triangular meshes that may have normal vectors and/or texture coordinates. Support for points and line segments may be added in a future release.

By default, the geometrical data is returned in the `ObjCoordinates` [coordinate system](https://github.com/ianmackenzie/elm-geometry#coordinate-systems).
It's also possible to [transform coordinates](#coordinate-conversion) if desired.

Note that all primitive decoders require at least one face and will fail if no faces are found.

@docs triangles, faces, texturedTriangles, texturedFaces, polylines


# Run Decoders

@docs decodeString, expectObj


# Filtering

@docs object, group, defaultGroup, material


# Metadata

@docs objectNames, groupNames, materialNames


# Mapping

@docs map, map2, map3, map4, map5


# Advanced Decoding

@docs filter, oneOf, fail, succeed, andThen, combine


# Coordinate Conversion

@docs ObjCoordinates

@docs trianglesIn, facesIn, texturedTrianglesIn, texturedFacesIn, polylinesIn

-}

import Array exposing (Array)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Http
import Length exposing (Length, Meters)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import Quantity exposing (Unitless)
import Set
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


{-| A value that knows how to decode information from
[the OBJ file format](https://en.wikipedia.org/wiki/Wavefront_.obj_file)
-}
type Decoder a
    = Decoder (VertexData -> List String -> List Group -> Result String a)


{-| Decode just the plain positions. Use with `Scene3d.Mesh.indexedTriangles` and `Scene3d.Mesh.indexedFacets` from elm-3d-scene.
-}
triangles : Decoder (TriangularMesh (Point3d Meters ObjCoordinates))
triangles =
    trianglesIn Frame3d.atOrigin


{-| Decode positions and normals. Use with `Scene3d.Mesh.indexedFaces`.
-}
faces : Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, normal : Vector3d Unitless ObjCoordinates })
faces =
    facesIn Frame3d.atOrigin


{-| Decode positions and [UV](https://learnopengl.com/Getting-started/Textures) (texture) coordinates.
Use with `Scene3d.Mesh.texturedTriangles` or `Scene3d.Mesh.texturedFacets`.
-}
texturedTriangles : Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, uv : ( Float, Float ) })
texturedTriangles =
    texturedTrianglesIn Frame3d.atOrigin


{-| Decode positions, UV and normals. Use with `Scene3d.Mesh.texturedFaces`.
-}
texturedFaces : Decoder (TriangularMesh { position : Point3d Meters ObjCoordinates, normal : Vector3d Unitless ObjCoordinates, uv : ( Float, Float ) })
texturedFaces =
    texturedFacesIn Frame3d.atOrigin


{-| Decode polylines.
-}
polylines : Decoder (List (Polyline3d Meters ObjCoordinates))
polylines =
    polylinesIn Frame3d.atOrigin



-- RUN DECODERS


{-| Run the decoder on the string. Takes a function, that knows
how to convert float coordinates into physical units.

    decodeString Length.centimeters texturedFaces string

-}
decodeString : (Float -> Length) -> Decoder a -> String -> Result String a
decodeString units (Decoder decode) content =
    decodeHelp units decode (String.lines content) 1 [] [] [] [] Nothing Nothing [ "default" ] [] []


{-| Load a mesh from [HTTP request](https://package.elm-lang.org/packages/elm/http/latest/).

    getMesh : Cmd Msg
    getMesh =
        Http.get
            { url = "Pod.obj"
            , expect =
                expectObj GotMesh
                    Length.centimeters
                    texturedFaces
            }

-}
expectObj : (Result Http.Error a -> msg) -> (Float -> Length) -> Decoder a -> Http.Expect msg
expectObj toMsg units decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case decodeString units decoder body of
                        Ok value ->
                            Ok value

                        Err string ->
                            Err (Http.BadBody string)



-- FILTERING


{-| Decode the data for the object. You may decode multiple objects from the same OBJ file.
Like the car base and car wheels.

    base =
        object "base" triangles

    wheels =
        object "wheels" triangles

-}
object : String -> Decoder a -> Decoder a
object name =
    filterHelp ("object '" ++ name ++ "'") (\properties -> properties.object == Just name)


{-| Decode the data for the certain group.
-}
group : String -> Decoder a -> Decoder a
group name =
    filterHelp ("group '" ++ name ++ "'") (\properties -> List.member name properties.groups)


{-| Decode the default group. This group has a special meaning,
all triangles are assigned to it if a group is not specified.

    defaultGroup =
        group "default"

-}
defaultGroup : Decoder a -> Decoder a
defaultGroup =
    group "default"


{-| Decode the material.
-}
material : String -> Decoder a -> Decoder a
material name =
    filterHelp ("material '" ++ name ++ "'") (\properties -> properties.material == Just name)



-- METADATA


{-| Decode all object names within the current filter.
-}
objectNames : Decoder (List String)
objectNames =
    Decoder
        (\_ _ elements ->
            elements
                |> List.foldl
                    (\(Group properties _ _) objectsSet ->
                        case properties.object of
                            Just obj ->
                                Set.insert obj objectsSet

                            Nothing ->
                                objectsSet
                    )
                    Set.empty
                |> Set.toList
                |> Result.Ok
        )


{-| Decode all group names within the current filter.
-}
groupNames : Decoder (List String)
groupNames =
    Decoder
        (\_ _ elements ->
            elements
                |> List.foldl
                    (\(Group properties _ _) groupsSet ->
                        List.foldl Set.insert groupsSet properties.groups
                    )
                    Set.empty
                |> Set.toList
                |> Result.Ok
        )


{-| Decode all material names within the current filter.
-}
materialNames : Decoder (List String)
materialNames =
    Decoder
        (\_ _ elements ->
            elements
                |> List.foldl
                    (\(Group properties _ _) materialsSet ->
                        case properties.material of
                            Just obj ->
                                Set.insert obj materialsSet

                            Nothing ->
                                materialsSet
                    )
                    Set.empty
                |> Set.toList
                |> Result.Ok
        )



-- MAPPING


{-| Transform the decoder. May be useful in case need to post process the mesh data.
-}
map : (a -> b) -> Decoder a -> Decoder b
map fn (Decoder decoder) =
    Decoder
        (\vertexData filters elements ->
            Result.map fn (decoder vertexData filters elements)
        )


{-| Join the result from two decoders. This lets you extract parts of the same OBJ file into separate meshes.

    carDecoder =
        map2
            (\wheels base ->
                { wheels = wheels
                , base = base
                }
            )
            wheelsDecoder
            baseDecoder

-}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 fn (Decoder decoderA) (Decoder decoderB) =
    Decoder
        (\vertexData filters elements ->
            Result.map2 fn
                (decoderA vertexData filters elements)
                (decoderB vertexData filters elements)
        )


{-| -}
map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 fn (Decoder decoderA) (Decoder decoderB) (Decoder decoderC) =
    Decoder
        (\vertexData filters elements ->
            Result.map3 fn
                (decoderA vertexData filters elements)
                (decoderB vertexData filters elements)
                (decoderC vertexData filters elements)
        )


{-| -}
map4 : (a -> b -> c -> d -> e) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e
map4 fn (Decoder decoderA) (Decoder decoderB) (Decoder decoderC) (Decoder decoderD) =
    Decoder
        (\vertexData filters elements ->
            Result.map4 fn
                (decoderA vertexData filters elements)
                (decoderB vertexData filters elements)
                (decoderC vertexData filters elements)
                (decoderD vertexData filters elements)
        )


{-| -}
map5 : (a -> b -> c -> d -> e -> f) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f
map5 fn (Decoder decoderA) (Decoder decoderB) (Decoder decoderC) (Decoder decoderD) (Decoder decoderE) =
    Decoder
        (\vertexData filters elements ->
            Result.map5 fn
                (decoderA vertexData filters elements)
                (decoderB vertexData filters elements)
                (decoderC vertexData filters elements)
                (decoderD vertexData filters elements)
                (decoderE vertexData filters elements)
        )



-- ADVANCED DECODING


{-| Filter what should be decoded. For example, to implement the `group` decoder:

    group name =
        filter
            (\properties ->
                List.member name properties.groups
            )

-}
filter :
    ({ groups : List String, object : Maybe String, material : Maybe String } -> Bool)
    -> Decoder a
    -> Decoder a
filter =
    filterHelp "<custom filter>"


filterHelp :
    String
    -> ({ groups : List String, object : Maybe String, material : Maybe String } -> Bool)
    -> Decoder a
    -> Decoder a
filterHelp name fn (Decoder decoder) =
    Decoder
        (\vertexData filters elements ->
            decoder vertexData
                (name :: filters)
                (List.filter
                    (\(Group properties _ _) -> fn properties)
                    elements
                )
        )


{-| Try a bunch of different decoders. You will get the result from the first one that succeeds.
-}
oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    Decoder
        (\vertexData filters elements ->
            oneOfHelp vertexData filters elements decoders []
        )


oneOfHelp : VertexData -> List String -> List Group -> List (Decoder a) -> List String -> Result String a
oneOfHelp vertexData filters elements decoders errors =
    case decoders of
        [] ->
            if errors == [] then
                Err "Empty oneOf decoder"

            else
                Err ("Failed oneOf decoder: '" ++ String.join "', '" (List.reverse errors) ++ "'.")

        (Decoder decoder) :: remainingDecoders ->
            case decoder vertexData filters elements of
                Ok res ->
                    Ok res

                Err error ->
                    oneOfHelp vertexData filters elements remainingDecoders (error :: errors)


{-| A decoder that always succeeds with the result. May be useful in combination with `oneOf` to
provide a placeholder mesh if decoding fails.
-}
succeed : a -> Decoder a
succeed mesh =
    Decoder (\_ _ _ -> Result.Ok mesh)


{-| A decoder that always fails with a given error message.
Use it in case you need custom error messages.
-}
fail : String -> Decoder a
fail error =
    Decoder (\_ _ _ -> Result.Err error)


{-| Run one decoder and then run another decoder. Useful when you first want to look at metadata, and then filter based on that.
-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen fn (Decoder decoderA) =
    Decoder
        (\vertexData filters elements ->
            case decoderA vertexData filters elements of
                Ok result ->
                    case fn result of
                        Decoder decoderB ->
                            decoderB vertexData [] elements

                Err error ->
                    Err error
        )


{-| Combine multiple decoders together. An example use case is when you want
to extract meshes for all materials:

    -- Decode triangles for selected materials
    trianglesForMaterials names =
        names
            |> List.map
                (\name ->
                    map (\triangles -> ( name, triangles ))
                        (material name triangles)
                )
            |> combine

    -- Decode materials, and then decode
    -- triangles for these materials
    withMaterials =
        materials |> andThen trianglesForMaterials

-}
combine : List (Decoder a) -> Decoder (List a)
combine decoders =
    Decoder
        (\vertexData filters elements ->
            combineHelp vertexData filters elements decoders []
        )


combineHelp : VertexData -> List String -> List Group -> List (Decoder a) -> List a -> Result String (List a)
combineHelp vertexData filters elements decoders list =
    case decoders of
        (Decoder decoder) :: remainingDecoders ->
            case decoder vertexData filters elements of
                Ok result ->
                    combineHelp vertexData filters elements remainingDecoders (result :: list)

                Err error ->
                    Err error

        [] ->
            Ok (List.reverse list)


{-| Coordinate system for decoded meshes.
-}
type ObjCoordinates
    = ObjCoordinates


{-| Transform coordinates when decoding. For example, if you need to render a mesh with Z-up,
but it was exported with Y-up:

    yUpToZUpFrame =
        Frame3d.atOrigin
            |> Frame3d.rotateAround
                Axis3d.x
                (Angle.degrees 90)

    yUpToZUpTriangles =
        trianglesIn yUpToZUpFrame

-}
trianglesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh (Point3d Meters coordinates))
trianglesIn frame =
    Decoder
        (\vertexData filters groups ->
            triangularMesh
                (addTriangles vertexData frame)
                filters
                groups
                (indexState vertexData.indexMap)
                []
        )


{-| -}
facesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates })
facesIn frame =
    Decoder
        (\vertexData filters groups ->
            triangularMesh
                (addFaces vertexData frame)
                filters
                groups
                (indexState vertexData.indexMap)
                []
        )


{-| -}
texturedTrianglesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh { position : Point3d Meters coordinates, uv : ( Float, Float ) })
texturedTrianglesIn frame =
    Decoder
        (\vertexData filters groups ->
            triangularMesh
                (addTexturedTriangles vertexData frame)
                filters
                groups
                (indexState vertexData.indexMap)
                []
        )


{-| -}
texturedFacesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ) })
texturedFacesIn frame =
    Decoder
        (\vertexData filters groups ->
            triangularMesh
                (addTexturedFaces vertexData frame)
                filters
                groups
                (indexState vertexData.indexMap)
                []
        )


{-| -}
polylinesIn : Frame3d Meters coordinates { defines : ObjCoordinates } -> Decoder (List (Polyline3d Meters coordinates))
polylinesIn frame =
    Decoder
        (\{ positions } filters groups ->
            addPolylines (PolylinesSettings positions frame filters)
                groups
                []
                0
                []
                []
                []
        )



-- Internals


type alias Face coordinates =
    { position : Point3d Meters coordinates
    , normal : Vector3d Unitless coordinates
    }


type alias TexturedTriangle coordinates =
    { position : Point3d Meters coordinates
    , uv : ( Float, Float )
    }


type alias TexturedFace coordinates =
    { position : Point3d Meters coordinates
    , normal : Vector3d Unitless coordinates
    , uv : ( Float, Float )
    }


type alias VertexData =
    { positions : Array (Point3d Meters ObjCoordinates)
    , normals : Array (Direction3d ObjCoordinates)
    , uvs : Array ( Float, Float )
    , indexMap : Array (List Int)
    }


type FaceElement
    = FaceElement Int (List Vertex)


type LineElement
    = LineElement Int (List Vertex)


type Vertex
    = Vertex Int (Maybe Int) (Maybe Int)


type Group
    = Group
        { groups : List String
        , object : Maybe String
        , material : Maybe String
        }
        (List FaceElement)
        (List LineElement)


type PropertyType
    = GroupsProperty (List String)
    | ObjectProperty String
    | MaterialProperty String


type Line
    = Property PropertyType
    | PositionData (Point3d Meters ObjCoordinates)
    | NormalData (Direction3d ObjCoordinates)
    | UvData ( Float, Float )
    | FaceElementData FaceElement
    | LineElementData LineElement
    | Error String
    | Skip


type alias IndexState a =
    { maxIndex : Int
    , indexMap : Array (List Int)
    , vertices : List a
    }


indexState : Array (List Int) -> IndexState a
indexState indexMap =
    { maxIndex = -1
    , indexMap = indexMap
    , vertices = []
    }


{-| Defines a function that knows how to collect a certain kind of triangle
-}
type alias AddIndexedTriangles a =
    Int
    -> List Vertex
    -> List FaceElement
    -> Int
    -> Array (List Int)
    -> List a
    -> List Int
    -> List ( Int, Int, Int )
    -> Result String ( IndexState a, List ( Int, Int, Int ) )


triangularMesh : AddIndexedTriangles a -> List String -> List Group -> IndexState a -> List ( Int, Int, Int ) -> Result String (TriangularMesh a)
triangularMesh add filters groups ({ maxIndex, indexMap, vertices } as currentIndexedState) faceIndices =
    case groups of
        (Group _ ((FaceElement lineno elementVertices) :: faceElements) _) :: remainingElementGroups ->
            case add lineno elementVertices faceElements maxIndex indexMap vertices [] faceIndices of
                Ok ( newIndexedState, newFaceIndices ) ->
                    triangularMesh add filters remainingElementGroups newIndexedState newFaceIndices

                Err error ->
                    Err error

        (Group _ [] _) :: remainingElementGroups ->
            -- skip an empty group
            triangularMesh add filters remainingElementGroups currentIndexedState faceIndices

        [] ->
            if faceIndices == [] then
                if filters == [] then
                    Err "No faces found"

                else
                    Err ("No faces found for " ++ String.join ", " filters)

            else
                Ok (TriangularMesh.indexed (Array.fromList (List.reverse vertices)) faceIndices)


groupIndices : Int -> List Int -> List ( Int, Int, Int ) -> List ( Int, Int, Int )
groupIndices p1 more result =
    case more of
        p2 :: rest ->
            case rest of
                p3 :: _ ->
                    -- Note that when it comes to grouping, the order of points is reversed
                    -- but the indices were reversed too, when parsing, so this is fine :-)
                    groupIndices p1 rest (( p1, p2, p3 ) :: result)

                _ ->
                    result

        [] ->
            result


addTriangles : VertexData -> Frame3d Meters coordinates { defines : ObjCoordinates } -> AddIndexedTriangles (Point3d Meters coordinates)
addTriangles vertexData frame lineno elementVertices elements maxIndex indexMap vertices indices faceIndices =
    case elementVertices of
        [] ->
            let
                newFaceIndices =
                    case indices of
                        p1 :: remainingIndices ->
                            -- parser guarantees at least 3 face indices
                            groupIndices p1 remainingIndices faceIndices

                        [] ->
                            faceIndices
            in
            case elements of
                (FaceElement newLineno newElementVertices) :: remainingElements ->
                    addTriangles vertexData
                        frame
                        newLineno
                        newElementVertices
                        remainingElements
                        maxIndex
                        indexMap
                        vertices
                        []
                        newFaceIndices

                [] ->
                    Ok ( { maxIndex = maxIndex, indexMap = indexMap, vertices = vertices }, newFaceIndices )

        (Vertex p _ _) :: remainingVertices ->
            case Array.get p indexMap of
                Just [ idx ] ->
                    addTriangles vertexData
                        frame
                        lineno
                        remainingVertices
                        elements
                        maxIndex
                        indexMap
                        vertices
                        (idx :: indices)
                        faceIndices

                _ ->
                    case Array.get p vertexData.positions of
                        Just vertex ->
                            addTriangles vertexData
                                frame
                                lineno
                                remainingVertices
                                elements
                                (maxIndex + 1)
                                (Array.set p [ maxIndex + 1 ] indexMap)
                                (Point3d.placeIn frame vertex :: vertices)
                                (maxIndex + 1 :: indices)
                                faceIndices

                        Nothing ->
                            formatError lineno "Index out of range"


addFaces : VertexData -> Frame3d Meters coordinates { defines : ObjCoordinates } -> AddIndexedTriangles (Face coordinates)
addFaces vertexData frame lineno elementVertices elements maxIndex indexMap vertices indices faceIndices =
    case elementVertices of
        [] ->
            let
                newFaceIndices =
                    case indices of
                        p1 :: remainingIndices ->
                            -- parser guarantees at least 3 face indices
                            groupIndices p1 remainingIndices faceIndices

                        [] ->
                            faceIndices
            in
            case elements of
                (FaceElement newLineno newElementVertices) :: remainingElements ->
                    addFaces vertexData
                        frame
                        newLineno
                        newElementVertices
                        remainingElements
                        maxIndex
                        indexMap
                        vertices
                        []
                        newFaceIndices

                [] ->
                    Ok ( { maxIndex = maxIndex, indexMap = indexMap, vertices = vertices }, newFaceIndices )

        (Vertex p _ (Just n)) :: remainingVertices ->
            let
                lookupArray =
                    Maybe.withDefault [] (Array.get p indexMap)
            in
            case lookup1 n lookupArray of
                Just idx ->
                    addFaces vertexData
                        frame
                        lineno
                        remainingVertices
                        elements
                        maxIndex
                        indexMap
                        vertices
                        (idx :: indices)
                        faceIndices

                Nothing ->
                    case
                        Maybe.map2
                            (\position normal ->
                                { position = Point3d.placeIn frame position
                                , normal = Direction3d.toVector (Direction3d.placeIn frame normal)
                                }
                            )
                            (Array.get p vertexData.positions)
                            (Array.get n vertexData.normals)
                    of
                        Just vertex ->
                            addFaces vertexData
                                frame
                                lineno
                                remainingVertices
                                elements
                                (maxIndex + 1)
                                (Array.set p (n :: maxIndex + 1 :: lookupArray) indexMap)
                                (vertex :: vertices)
                                (maxIndex + 1 :: indices)
                                faceIndices

                        Nothing ->
                            formatError lineno "Index out of range"

        (Vertex _ _ Nothing) :: _ ->
            formatError lineno "Vertex has no normal vector"


addTexturedTriangles : VertexData -> Frame3d Meters coordinates { defines : ObjCoordinates } -> AddIndexedTriangles (TexturedTriangle coordinates)
addTexturedTriangles vertexData frame lineno elementVertices elements maxIndex indexMap vertices indices faceIndices =
    case elementVertices of
        [] ->
            let
                newFaceIndices =
                    case indices of
                        p1 :: remainingIndices ->
                            -- parser guarantees at least 3 face indices
                            groupIndices p1 remainingIndices faceIndices

                        [] ->
                            faceIndices
            in
            case elements of
                (FaceElement newLineno newElementVertices) :: remainingElements ->
                    addTexturedTriangles vertexData
                        frame
                        newLineno
                        newElementVertices
                        remainingElements
                        maxIndex
                        indexMap
                        vertices
                        []
                        newFaceIndices

                [] ->
                    Ok ( { maxIndex = maxIndex, indexMap = indexMap, vertices = vertices }, newFaceIndices )

        (Vertex p (Just uv) _) :: remainingVertices ->
            let
                lookupArray =
                    Maybe.withDefault [] (Array.get p indexMap)
            in
            case lookup1 uv lookupArray of
                Just idx ->
                    addTexturedTriangles vertexData
                        frame
                        lineno
                        remainingVertices
                        elements
                        maxIndex
                        indexMap
                        vertices
                        (idx :: indices)
                        faceIndices

                Nothing ->
                    case
                        Maybe.map2
                            (\position uvCoord ->
                                { position = Point3d.placeIn frame position
                                , uv = uvCoord
                                }
                            )
                            (Array.get p vertexData.positions)
                            (Array.get uv vertexData.uvs)
                    of
                        Just vertex ->
                            addTexturedTriangles vertexData
                                frame
                                lineno
                                remainingVertices
                                elements
                                (maxIndex + 1)
                                (Array.set p (uv :: maxIndex + 1 :: lookupArray) indexMap)
                                (vertex :: vertices)
                                (maxIndex + 1 :: indices)
                                faceIndices

                        Nothing ->
                            formatError lineno "Index out of range"

        (Vertex _ Nothing _) :: _ ->
            formatError lineno "Vertex has no texture coordinates"


addTexturedFaces : VertexData -> Frame3d Meters coordinates { defines : ObjCoordinates } -> AddIndexedTriangles (TexturedFace coordinates)
addTexturedFaces vertexData frame lineno elementVertices elements maxIndex indexMap vertices indices faceIndices =
    case elementVertices of
        [] ->
            let
                newFaceIndices =
                    case indices of
                        p1 :: remainingIndices ->
                            -- parser guarantees at least 3 face indices
                            groupIndices p1 remainingIndices faceIndices

                        [] ->
                            faceIndices
            in
            case elements of
                (FaceElement newLineno newElementVertices) :: remainingElements ->
                    addTexturedFaces vertexData
                        frame
                        newLineno
                        newElementVertices
                        remainingElements
                        maxIndex
                        indexMap
                        vertices
                        []
                        newFaceIndices

                [] ->
                    Ok ( { maxIndex = maxIndex, indexMap = indexMap, vertices = vertices }, newFaceIndices )

        (Vertex p (Just uv) (Just n)) :: remainingVertices ->
            let
                lookupArray =
                    Maybe.withDefault [] (Array.get p indexMap)
            in
            case lookup2 uv n lookupArray of
                Just idx ->
                    addTexturedFaces vertexData
                        frame
                        lineno
                        remainingVertices
                        elements
                        maxIndex
                        indexMap
                        vertices
                        (idx :: indices)
                        faceIndices

                Nothing ->
                    case
                        Maybe.map3
                            (\position normal uvCoord ->
                                { position = Point3d.placeIn frame position
                                , normal = Direction3d.toVector (Direction3d.placeIn frame normal)
                                , uv = uvCoord
                                }
                            )
                            (Array.get p vertexData.positions)
                            (Array.get n vertexData.normals)
                            (Array.get uv vertexData.uvs)
                    of
                        Just vertex ->
                            addTexturedFaces vertexData
                                frame
                                lineno
                                remainingVertices
                                elements
                                (maxIndex + 1)
                                (Array.set p (uv :: n :: maxIndex + 1 :: lookupArray) indexMap)
                                (vertex :: vertices)
                                (maxIndex + 1 :: indices)
                                faceIndices

                        Nothing ->
                            formatError lineno "Index out of range"

        _ ->
            formatError lineno "Vertex missing normal vector and/or texture coordinates"


lookup2 : Int -> Int -> List Int -> Maybe Int
lookup2 idx1 idx2 list =
    case list of
        i1 :: i2 :: result :: rest ->
            if idx1 - i1 == 0 && idx2 - i2 == 0 then
                Just result

            else
                lookup2 idx1 idx2 rest

        _ ->
            Nothing


lookup1 : Int -> List Int -> Maybe Int
lookup1 idx1 list =
    case list of
        i1 :: result :: rest ->
            if idx1 - i1 == 0 then
                Just result

            else
                lookup1 idx1 rest

        _ ->
            Nothing


type alias PolylinesSettings coordinates =
    { positions : Array (Point3d Meters ObjCoordinates)
    , frame : Frame3d Meters coordinates { defines : ObjCoordinates }
    , filters : List String
    }


addPolylines :
    PolylinesSettings coordinates
    -> List Group
    -> List LineElement
    -> Int
    -> List Vertex
    -> List (Point3d Meters coordinates)
    -> List (Polyline3d Meters coordinates)
    -> Result String (List (Polyline3d Meters coordinates))
addPolylines settings groups elements lineno vertices points result =
    case vertices of
        (Vertex p _ _) :: remainingVertices ->
            case Array.get p settings.positions of
                Just point ->
                    addPolylines settings
                        groups
                        elements
                        lineno
                        remainingVertices
                        (Point3d.placeIn settings.frame point :: points)
                        result

                Nothing ->
                    formatError lineno "Index out of range"

        [] ->
            let
                newResult =
                    if points == [] then
                        result

                    else
                        -- the points are reversed, but the original indices
                        -- were reversed too in the parser
                        Polyline3d.fromVertices points :: result
            in
            case elements of
                (LineElement newLineno newVertices) :: remainingElements ->
                    addPolylines settings
                        groups
                        remainingElements
                        newLineno
                        newVertices
                        []
                        newResult

                [] ->
                    case groups of
                        (Group _ _ newElements) :: remainingGroups ->
                            addPolylines settings
                                remainingGroups
                                newElements
                                0
                                []
                                []
                                newResult

                        [] ->
                            if newResult == [] then
                                if settings.filters == [] then
                                    Err "No lines found"

                                else
                                    Err ("No lines found for " ++ String.join ", " settings.filters)

                            else
                                Ok newResult


decodeHelp :
    (Float -> Length)
    -> (VertexData -> List String -> List Group -> Result String a)
    -> List String
    -> Int
    -> List (Point3d Meters ObjCoordinates)
    -> List (Direction3d ObjCoordinates)
    -> List ( Float, Float )
    -> List Group
    -> Maybe String
    -> Maybe String
    -> List String
    -> List FaceElement
    -> List LineElement
    -> Result String a
decodeHelp units decode lines lineno positions normals uvs groups object_ material_ groups_ faceElements lineElements =
    case lines of
        [] ->
            let
                positions_ =
                    Array.fromList (List.reverse positions)
            in
            decode
                { positions = positions_
                , normals = Array.fromList (List.reverse normals)
                , uvs = Array.fromList (List.reverse uvs)
                , indexMap = Array.repeat (Array.length positions_) []
                }
                []
                (if faceElements == [] && lineElements == [] then
                    groups

                 else
                    -- flush the last group
                    Group { groups = groups_, object = object_, material = material_ } faceElements lineElements :: groups
                )

        line :: remainingLines ->
            case parseLine lineno units line of
                Property propertyType ->
                    let
                        newElementGroups =
                            if faceElements == [] && lineElements == [] then
                                groups

                            else
                                Group { groups = groups_, object = object_, material = material_ } faceElements lineElements :: groups
                    in
                    case propertyType of
                        GroupsProperty newGroups ->
                            decodeHelp units decode remainingLines (lineno + 1) positions normals uvs newElementGroups object_ material_ newGroups [] []

                        ObjectProperty newObject ->
                            decodeHelp units decode remainingLines (lineno + 1) positions normals uvs newElementGroups (Just newObject) material_ groups_ [] []

                        MaterialProperty newMaterial ->
                            decodeHelp units decode remainingLines (lineno + 1) positions normals uvs newElementGroups object_ (Just newMaterial) groups_ [] []

                PositionData position ->
                    decodeHelp units decode remainingLines (lineno + 1) (position :: positions) normals uvs groups object_ material_ groups_ faceElements lineElements

                NormalData normal ->
                    decodeHelp units decode remainingLines (lineno + 1) positions (normal :: normals) uvs groups object_ material_ groups_ faceElements lineElements

                UvData uv ->
                    decodeHelp units decode remainingLines (lineno + 1) positions normals (uv :: uvs) groups object_ material_ groups_ faceElements lineElements

                FaceElementData faceElement ->
                    decodeHelp units decode remainingLines (lineno + 1) positions normals uvs groups object_ material_ groups_ (faceElement :: faceElements) lineElements

                LineElementData lineElement ->
                    decodeHelp units decode remainingLines (lineno + 1) positions normals uvs groups object_ material_ groups_ faceElements (lineElement :: lineElements)

                Skip ->
                    decodeHelp units decode remainingLines (lineno + 1) positions normals uvs groups object_ material_ groups_ faceElements lineElements

                Error error ->
                    formatError lineno error


parseLine : Int -> (Float -> Length) -> String -> Line
parseLine lineno units line =
    if String.startsWith "o " line then
        case String.trim (String.dropLeft 2 line) of
            "" ->
                Error "No object name"

            object_ ->
                Property (ObjectProperty object_)

    else if String.startsWith "g " line then
        case String.words (String.dropLeft 2 line) of
            [] ->
                Error "No groups specified"

            groups_ ->
                Property (GroupsProperty groups_)

    else if String.startsWith "usemtl " line then
        case String.trim (String.dropLeft 7 line) of
            "" ->
                Error "No material name"

            material_ ->
                Property (MaterialProperty material_)

    else if String.startsWith "v " line then
        case List.map String.toFloat (String.words (String.dropLeft 2 line)) of
            [ Just x, Just y, Just z ] ->
                PositionData (Point3d.xyz (units x) (units y) (units z))

            _ ->
                Error "Invalid position format"

    else if String.startsWith "vt " line then
        case List.map String.toFloat (String.words (String.dropLeft 3 line)) of
            [ Just x, Just y ] ->
                UvData ( x, y )

            _ ->
                Error "Invalid texture coordinates format"

    else if String.startsWith "vn " line then
        case List.map String.toFloat (String.words (String.dropLeft 3 line)) of
            [ Just x, Just y, Just z ] ->
                NormalData (Direction3d.unsafe { x = x, y = y, z = z })

            _ ->
                Error "Invalid normal vector format"

    else if String.startsWith "f " line then
        case parseVertices (String.words (String.dropLeft 2 line)) [] of
            Just [] ->
                Error "Face has no vertices"

            Just [ _ ] ->
                Error "Face has less than three vertices"

            Just [ _, _ ] ->
                Error "Face has less than three vertices"

            Just vertices ->
                FaceElementData (FaceElement lineno vertices)

            Nothing ->
                Error "Invalid face format"

    else if String.startsWith "l " line then
        case parseVertices (String.words (String.dropLeft 2 line)) [] of
            Just [] ->
                Error "Line has no vertices"

            Just [ _ ] ->
                Error "Line has less than two vertices"

            Just vertices ->
                LineElementData (LineElement lineno vertices)

            Nothing ->
                Error "Invalid line format"

    else
        Skip


parseVertices : List String -> List Vertex -> Maybe (List Vertex)
parseVertices list vertices =
    case list of
        first :: more ->
            case List.map String.toInt (String.split "/" first) of
                [ Just p ] ->
                    parseVertices more
                        (Vertex (p - 1) Nothing Nothing :: vertices)

                [ Just p, Just uv ] ->
                    parseVertices more
                        (Vertex (p - 1) (Just (uv - 1)) Nothing :: vertices)

                [ Just p, uv, Just n ] ->
                    parseVertices more
                        (Vertex (p - 1) (Maybe.map (\x -> x - 1) uv) (Just (n - 1)) :: vertices)

                _ ->
                    Nothing

        [] ->
            -- Note that this reverses vertices
            Just vertices


formatError : Int -> String -> Result String a
formatError lineno error =
    Err ("Line " ++ String.fromInt lineno ++ ": " ++ error)
