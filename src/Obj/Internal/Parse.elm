module Obj.Internal.Parse exposing
    ( FaceElement(..)
    , Group(..)
    , LineElement(..)
    , PointsElement(..)
    , Vertex
    , VertexData
    , formatError
    , parse
    )

import Array exposing (Array)
import Direction3d exposing (Direction3d)
import Length exposing (Meters)
import Obj.Internal.IndexMap as IndexMap exposing (Empty, IndexMap)
import Point3d exposing (Point3d)


type alias VertexData coordinates =
    { positions : Array (Point3d Meters coordinates)
    , normals : Array (Direction3d coordinates)
    , uvs : Array ( Float, Float )
    , emptyIndexMap : IndexMap Empty
    , fullGroups : List Group
    }


type FaceElement
    = FaceElement Int Bool (List Vertex)


type LineElement
    = LineElement Int (List Vertex)


type PointsElement
    = PointsElement Int (List Vertex)


{-| Stores indices into positions, uv coordinates and normal.
Position index is always there. We use -1 for the missing uv or normal index.
-}
type alias Vertex =
    { p : Int, uv : Int, n : Int }


type Group
    = Group
        { groups : List String
        , object : Maybe String
        , material : Maybe String
        , smoothingGroup : Int
        }
        (List FaceElement)
        (List LineElement)
        (List PointsElement)


formatError : Int -> String -> Result String a
formatError lineno error =
    Err ("Line " ++ String.fromInt lineno ++ ": " ++ error)


parse : (Float -> Float) -> String -> Result String ( VertexData coordinates, List Group )
parse units content =
    parseHelp units (String.lines content) 1 [] [] [] [] Nothing Nothing [ "default" ] [] [] [] 0


parseHelp :
    (Float -> Float)
    -> List String
    -> Int
    -> List (Point3d Meters coordinates)
    -> List (Direction3d.Direction3d coordinates)
    -> List ( Float, Float )
    -> List Group
    -> Maybe String
    -> Maybe String
    -> List String
    -> List FaceElement
    -> List LineElement
    -> List PointsElement
    -> Int
    -> Result String ( VertexData coordinates, List Group )
parseHelp units lines lineno positions normals uvs groups object_ material_ groups_ faceElements lineElements pointsElements currentSmoothingGroup =
    case lines of
        line :: remainingLines ->
            -- conditions are sorted based on the frequency of occurrence
            case String.left 2 line of
                "f " ->
                    case parseFaceElements lineno lines faceElements of
                        Ok ( newLineno, newLines, newFaceElements ) ->
                            parseHelp units newLines newLineno positions normals uvs groups object_ material_ groups_ newFaceElements lineElements pointsElements currentSmoothingGroup

                        Err err ->
                            Err err

                "v " ->
                    case parsePositions units lineno lines positions of
                        Ok ( newLineno, newLines, newPositions ) ->
                            parseHelp units newLines newLineno newPositions normals uvs groups object_ material_ groups_ faceElements lineElements pointsElements currentSmoothingGroup

                        Err err ->
                            Err err

                "vt" ->
                    -- we can commit to this path because no other command starts with "vt"
                    case parseUvs lineno lines uvs of
                        Ok ( newLineno, newLines, newUvs ) ->
                            parseHelp units newLines newLineno positions normals newUvs groups object_ material_ groups_ faceElements lineElements pointsElements currentSmoothingGroup

                        Err err ->
                            Err err

                "vn" ->
                    -- we can commit to this path because no other command starts with "vn"
                    case parseNormals lineno lines normals of
                        Ok ( newLineno, newLines, newNormals ) ->
                            parseHelp units newLines newLineno positions newNormals uvs groups object_ material_ groups_ faceElements lineElements pointsElements currentSmoothingGroup

                        Err err ->
                            Err err

                _ ->
                    case String.words line of
                        "o" :: rest ->
                            case rest of
                                newObject :: _ ->
                                    parseHelp units remainingLines (lineno + 1) positions normals uvs (addNonEmptyGroup object_ material_ groups_ currentSmoothingGroup faceElements lineElements pointsElements groups) (Just newObject) material_ groups_ [] [] [] currentSmoothingGroup

                                [] ->
                                    formatError lineno "No object name"

                        "g" :: newGroups ->
                            case newGroups of
                                [] ->
                                    parseHelp units remainingLines (lineno + 1) positions normals uvs (addNonEmptyGroup object_ material_ groups_ currentSmoothingGroup faceElements lineElements pointsElements groups) object_ material_ [ "default" ] [] [] [] currentSmoothingGroup

                                _ ->
                                    parseHelp units remainingLines (lineno + 1) positions normals uvs (addNonEmptyGroup object_ material_ groups_ currentSmoothingGroup faceElements lineElements pointsElements groups) object_ material_ newGroups [] [] [] currentSmoothingGroup

                        "usemtl" :: rest ->
                            case rest of
                                newMaterial :: _ ->
                                    parseHelp units remainingLines (lineno + 1) positions normals uvs groups object_ (Just newMaterial) groups_ faceElements lineElements pointsElements currentSmoothingGroup

                                [] ->
                                    formatError lineno "No material name"

                        "s" :: rest ->
                            let
                                newSmoothingGroup =
                                    case rest of
                                        [ "off" ] ->
                                            0

                                        [ numStr ] ->
                                            String.toInt numStr |> Maybe.withDefault 0

                                        _ ->
                                            0
                            in
                            parseHelp units remainingLines (lineno + 1) positions normals uvs (addNonEmptyGroup object_ material_ groups_ currentSmoothingGroup faceElements [] [] groups) object_ material_ groups_ [] lineElements pointsElements newSmoothingGroup

                        "l" :: _ ->
                            case parseLineElements lineno lines lineElements of
                                Ok ( newLineno, newLines, newLineElements ) ->
                                    parseHelp units newLines newLineno positions normals uvs groups object_ material_ groups_ faceElements newLineElements pointsElements currentSmoothingGroup

                                Err err ->
                                    Err err

                        "p" :: _ ->
                            case parsePointsElements lineno lines pointsElements of
                                Ok ( newLineno, newLines, newPointsElements ) ->
                                    parseHelp units newLines newLineno positions normals uvs groups object_ material_ groups_ faceElements lineElements newPointsElements currentSmoothingGroup

                                Err err ->
                                    Err err

                        "" :: _ ->
                            -- skip empty lines
                            parseHelp units remainingLines (lineno + 1) positions normals uvs groups object_ material_ groups_ faceElements lineElements pointsElements currentSmoothingGroup

                        command :: _ ->
                            if String.left 1 command == "#" || List.member command skipCommands then
                                -- Skip unsupported commands and comments
                                parseHelp units remainingLines (lineno + 1) positions normals uvs groups object_ material_ groups_ faceElements lineElements pointsElements currentSmoothingGroup

                            else
                                formatError lineno
                                    ("Invalid OBJ syntax '"
                                        ++ (if String.length line > 20 then
                                                String.left 20 line ++ "...'"

                                            else
                                                line ++ "'"
                                           )
                                    )

                        [] ->
                            -- This is an impossible case, because String.words always returns at least one element, for empty lines it is [""]
                            parseHelp units remainingLines (lineno + 1) positions normals uvs groups object_ material_ groups_ faceElements lineElements pointsElements currentSmoothingGroup

        [] ->
            let
                positionsArray =
                    Array.fromList (List.reverse positions)

                fullGroups =
                    -- flush the last group
                    addNonEmptyGroup object_ material_ groups_ currentSmoothingGroup faceElements lineElements pointsElements groups
            in
            Ok
                ( { positions = positionsArray
                  , normals = Array.fromList (List.reverse normals)
                  , uvs = Array.fromList (List.reverse uvs)
                  , emptyIndexMap = IndexMap.empty (Array.length positionsArray)
                  , fullGroups = fullGroups
                  }
                , fullGroups
                )


addNonEmptyGroup : Maybe String -> Maybe String -> List String -> Int -> List FaceElement -> List LineElement -> List PointsElement -> List Group -> List Group
addNonEmptyGroup object_ material_ groups_ smoothingGroup faceElements lineElements pointsElements groups =
    case faceElements of
        _ :: _ ->
            Group { groups = groups_, object = object_, material = material_, smoothingGroup = smoothingGroup } faceElements lineElements pointsElements :: groups

        [] ->
            case lineElements of
                _ :: _ ->
                    Group { groups = groups_, object = object_, material = material_, smoothingGroup = smoothingGroup } faceElements lineElements pointsElements :: groups

                [] ->
                    case pointsElements of
                        _ :: _ ->
                            Group { groups = groups_, object = object_, material = material_, smoothingGroup = smoothingGroup } faceElements lineElements pointsElements :: groups

                        [] ->
                            groups


skipCommands : List String
skipCommands =
    [ -- Grouping
      "mg" -- merging group

    -- Display/render attributes
    , "mtllib" -- material library
    , "bevel" -- bevel interpolation
    , "c_interp" -- color interpolation
    , "d_interp" -- dissolve interpolation
    , "lod" -- level of detail
    , "shadow_obj" -- shadow casting
    , "trace_obj" -- ray tracing
    , "ctech" -- curve approximation technique
    , "stech" -- surface approximation technique

    -- Free-form curve/surface attributes
    , "cstype" -- forms of curve or surface type
    , "deg" -- degree
    , "bmat" -- basis matrix
    , "step" -- step size

    -- Elements
    , "curv" -- curve
    , "curv2" -- 2D curve
    , "surf" -- surface

    -- Free-form curve/surface body statements
    , "parm" -- parameter values
    , "trim" -- outer trimming loop
    , "hole" -- inner trimming loop
    , "scrv" -- special curve
    , "sp" -- special point
    , "end" -- end statement

    -- Connectivity between free-form surfaces
    , "con" -- connect

    -- General statement
    , "call"
    , "scmp"
    , "csh"
    ]


parsePositions : (Float -> Float) -> Int -> List String -> List (Point3d Meters coordinates) -> Result String ( Int, List String, List (Point3d Meters coordinates) )
parsePositions units lineno lines positions =
    case lines of
        line :: remainingLines ->
            case String.words line of
                "v" :: coords ->
                    -- sometimes position has more than 3 components, with the 4th component
                    -- being the optional weight, that is only required for rational curves and surfaces
                    -- we ignore everything after x y z for performance
                    case coords of
                        sx :: sy :: sz :: _ ->
                            case String.toFloat sx of
                                Just x ->
                                    case String.toFloat sy of
                                        Just y ->
                                            case String.toFloat sz of
                                                Just z ->
                                                    parsePositions units
                                                        (lineno + 1)
                                                        remainingLines
                                                        (Point3d.fromMeters
                                                            { x = units x
                                                            , y = units y
                                                            , z = units z
                                                            }
                                                            :: positions
                                                        )

                                                Nothing ->
                                                    formatError lineno "Invalid position format"

                                        Nothing ->
                                            formatError lineno "Invalid position format"

                                Nothing ->
                                    formatError lineno "Invalid position format"

                        _ ->
                            formatError lineno "Invalid position format"

                _ ->
                    Ok ( lineno, lines, positions )

        [] ->
            Ok ( lineno, lines, positions )


parseUvs : Int -> List String -> List ( Float, Float ) -> Result String ( Int, List String, List ( Float, Float ) )
parseUvs lineno lines uvs =
    case lines of
        line :: remainingLines ->
            case String.words line of
                "vt" :: coords ->
                    case coords of
                        -- sometimes uv has more than 2 components, with the 3rd component
                        -- being the optional depth of the texture
                        -- we ignore everything after u v for performance
                        su :: sv :: _ ->
                            case String.toFloat su of
                                Just u ->
                                    case String.toFloat sv of
                                        Just v ->
                                            parseUvs (lineno + 1) remainingLines (( u, v ) :: uvs)

                                        Nothing ->
                                            formatError lineno "Invalid texture coordinates format"

                                Nothing ->
                                    formatError lineno "Invalid texture coordinates format"

                        su :: [] ->
                            -- set the default v=0 if it is missing
                            case String.toFloat su of
                                Just u ->
                                    parseUvs (lineno + 1) remainingLines (( u, 0 ) :: uvs)

                                Nothing ->
                                    formatError lineno "Invalid texture coordinates format"

                        _ ->
                            formatError lineno "Invalid texture coordinates format"

                _ ->
                    Ok ( lineno, lines, uvs )

        [] ->
            Ok ( lineno, lines, uvs )


parseNormals : Int -> List String -> List (Direction3d.Direction3d coordinates) -> Result String ( Int, List String, List (Direction3d.Direction3d coordinates) )
parseNormals lineno lines normals =
    case lines of
        line :: remainingLines ->
            case String.words line of
                "vn" :: coords ->
                    case coords of
                        -- we ignore everything after x y z for performance
                        sx :: sy :: sz :: _ ->
                            case String.toFloat sx of
                                Just x ->
                                    case String.toFloat sy of
                                        Just y ->
                                            case String.toFloat sz of
                                                Just z ->
                                                    parseNormals (lineno + 1)
                                                        remainingLines
                                                        (Direction3d.unsafe
                                                            { x = x
                                                            , y = y
                                                            , z = z
                                                            }
                                                            :: normals
                                                        )

                                                Nothing ->
                                                    formatError lineno "Invalid normal vector format"

                                        Nothing ->
                                            formatError lineno "Invalid normal vector format"

                                Nothing ->
                                    formatError lineno "Invalid normal vector format"

                        _ ->
                            formatError lineno "Invalid normal vector format"

                _ ->
                    Ok ( lineno, lines, normals )

        [] ->
            Ok ( lineno, lines, normals )


parseFaceElements : Int -> List String -> List FaceElement -> Result String ( Int, List String, List FaceElement )
parseFaceElements lineno lines faceElements =
    case lines of
        line :: remainingLines ->
            case String.words line of
                "f" :: indices ->
                    let
                        ( hasNormals, vertices ) =
                            parseIndices indices True []
                    in
                    case vertices of
                        _ :: _ :: _ :: _ ->
                            parseFaceElements (lineno + 1)
                                remainingLines
                                (FaceElement lineno hasNormals vertices :: faceElements)

                        _ :: _ ->
                            formatError lineno "Face has less than three vertices"

                        [] ->
                            case indices of
                                [] ->
                                    formatError lineno "Face has less than three vertices"

                                _ ->
                                    formatError lineno "Invalid face format"

                _ ->
                    Ok ( lineno, lines, faceElements )

        [] ->
            Ok ( lineno, lines, faceElements )


parseLineElements : Int -> List String -> List LineElement -> Result String ( Int, List String, List LineElement )
parseLineElements lineno lines lineElements =
    case lines of
        line :: remainingLines ->
            case String.words line of
                "l" :: indices ->
                    let
                        ( _, vertices ) =
                            parseIndices indices False []
                    in
                    case vertices of
                        _ :: _ :: _ ->
                            parseLineElements (lineno + 1)
                                remainingLines
                                (LineElement lineno vertices :: lineElements)

                        _ :: _ ->
                            formatError lineno "Line has less than two vertices"

                        [] ->
                            case indices of
                                [] ->
                                    formatError lineno "Line has less than two vertices"

                                _ ->
                                    formatError lineno "Invalid line format"

                _ ->
                    Ok ( lineno, lines, lineElements )

        [] ->
            Ok ( lineno, lines, lineElements )


parsePointsElements : Int -> List String -> List PointsElement -> Result String ( Int, List String, List PointsElement )
parsePointsElements lineno lines pointsElements =
    case lines of
        line :: remainingLines ->
            case String.words line of
                "p" :: indices ->
                    let
                        ( _, vertices ) =
                            parseIndices indices False []
                    in
                    case vertices of
                        _ :: _ ->
                            parsePointsElements (lineno + 1)
                                remainingLines
                                (PointsElement lineno vertices :: pointsElements)

                        [] ->
                            case indices of
                                [] ->
                                    formatError lineno "Points element has no vertices"

                                _ ->
                                    formatError lineno "Invalid points format"

                _ ->
                    Ok ( lineno, lines, pointsElements )

        [] ->
            Ok ( lineno, lines, pointsElements )


parseIndices : List String -> Bool -> List Vertex -> ( Bool, List Vertex )
parseIndices list allHaveNormals vertices =
    case list of
        first :: more ->
            case String.split "/" first of
                pComponent :: uvnComponents ->
                    case String.toInt pComponent of
                        Just p ->
                            case uvnComponents of
                                uvComponent :: nComponents ->
                                    case String.toInt uvComponent of
                                        Just uv ->
                                            case nComponents of
                                                nComponent :: _ ->
                                                    case String.toInt nComponent of
                                                        Just n ->
                                                            parseIndices more
                                                                allHaveNormals
                                                                ({ p = p - 1, uv = uv - 1, n = n - 1 } :: vertices)

                                                        Nothing ->
                                                            ( False, [] )

                                                [] ->
                                                    parseIndices more
                                                        False
                                                        ({ p = p - 1, uv = uv - 1, n = -1 } :: vertices)

                                        Nothing ->
                                            case nComponents of
                                                nComponent :: _ ->
                                                    case String.toInt nComponent of
                                                        Just n ->
                                                            parseIndices more
                                                                allHaveNormals
                                                                ({ p = p - 1, uv = -1, n = n - 1 } :: vertices)

                                                        Nothing ->
                                                            ( False, [] )

                                                [] ->
                                                    parseIndices more
                                                        False
                                                        ({ p = p - 1, uv = -1, n = -1 } :: vertices)

                                [] ->
                                    parseIndices more
                                        False
                                        ({ p = p - 1, uv = -1, n = -1 } :: vertices)

                        Nothing ->
                            ( False, [] )

                [] ->
                    ( False, [] )

        [] ->
            -- Note that this reverses vertices
            ( allHaveNormals, vertices )
