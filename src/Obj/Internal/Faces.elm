module Obj.Internal.Faces exposing
    ( bumpyFaces
    , faces
    , texturedFaces
    )

import Array exposing (Array)
import Bitwise
import Dict exposing (Dict)
import Direction3d
import Frame3d exposing (Frame3d)
import Length exposing (Meters)
import Obj.Internal.MeshHelpers exposing (buildMeshResult, groupIndices, lookup1, lookup2)
import Obj.Internal.Parse
    exposing
        ( FaceElement(..)
        , Group(..)
        , ObjCoordinates
        , Vertex
        , VertexData
        , formatError
        )
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..), Unitless)
import Set exposing (Set)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


faces :
    Frame3d Meters coordinates { defines : ObjCoordinates }
    -> Bool
    -> VertexData
    -> List String
    -> List Group
    -> Result String (TriangularMesh (Face coordinates))
faces frame bitflags vertexData filters filteredGroups =
    case triangularMesh (addFaces frame vertexData) filteredGroups -1 vertexData.indexMap [] [] [] of
        Err e ->
            Err e

        Ok state ->
            case state.deferredFaces of
                [] ->
                    buildMeshResult filters (Array.fromList (List.reverse state.vertices)) state.faceIndices

                _ ->
                    let
                        weightedNormals =
                            if bitflags then
                                collectBitflagWeightedNormals vertexData filteredGroups Dict.empty (smoothingGroupsMask filteredGroups)

                            else
                                collectWeightedNormals vertexData filteredGroups Dict.empty (smoothingGroups filteredGroups)

                        ( verts, faceAcc ) =
                            generateFacesNormals frame vertexData state.deferredFaces Dict.empty state.vertices (state.maxIndex + 1) state.faceIndices weightedNormals bitflags
                    in
                    buildMeshResult filters (Array.fromList (List.reverse verts)) faceAcc


texturedFaces :
    Frame3d Meters coordinates { defines : ObjCoordinates }
    -> Bool
    -> VertexData
    -> List String
    -> List Group
    -> Result String (TriangularMesh (TexturedFace coordinates))
texturedFaces frame bitflags vertexData filters filteredGroups =
    case triangularMesh (addTexturedFaces frame vertexData) filteredGroups -1 vertexData.indexMap [] [] [] of
        Err e ->
            Err e

        Ok state ->
            case state.deferredFaces of
                [] ->
                    buildMeshResult filters (Array.fromList (List.reverse state.vertices)) state.faceIndices

                _ ->
                    let
                        weightedNormals =
                            if bitflags then
                                collectBitflagWeightedNormals vertexData filteredGroups Dict.empty (smoothingGroupsMask filteredGroups)

                            else
                                collectWeightedNormals vertexData filteredGroups Dict.empty (smoothingGroups filteredGroups)

                        ( verts, faceIndices ) =
                            generateTexturedFacesNormals frame vertexData state.deferredFaces Dict.empty state.vertices (state.maxIndex + 1) state.faceIndices weightedNormals bitflags
                    in
                    buildMeshResult filters (Array.fromList (List.reverse verts)) faceIndices


bumpyFaces :
    Frame3d Meters coordinates { defines : ObjCoordinates }
    -> Bool
    -> VertexData
    -> List String
    -> List Group
    -> Result String (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ), tangent : Vector3d Unitless coordinates, tangentBasisIsRightHanded : Bool })
bumpyFaces frame bitflags vertexData filters filteredGroups =
    case triangularMesh (addTexturedFaces frame vertexData) filteredGroups -1 vertexData.indexMap [] [] [] of
        Err e ->
            Err e

        Ok state ->
            case state.deferredFaces of
                [] ->
                    computeTangents filters state.vertices state.faceIndices

                _ ->
                    let
                        weightedNormals =
                            if bitflags then
                                collectBitflagWeightedNormals vertexData filteredGroups Dict.empty (smoothingGroupsMask filteredGroups)

                            else
                                collectWeightedNormals vertexData filteredGroups Dict.empty (smoothingGroups filteredGroups)

                        ( verts, faceIndices ) =
                            generateTexturedFacesNormals frame vertexData state.deferredFaces Dict.empty state.vertices (state.maxIndex + 1) state.faceIndices weightedNormals bitflags
                    in
                    computeTangents filters verts faceIndices


type alias Face coordinates =
    { position : Point3d Meters coordinates
    , normal : Vector3d Unitless coordinates
    }


type alias TexturedFace coordinates =
    { position : Point3d Meters coordinates
    , normal : Vector3d Unitless coordinates
    , uv : ( Float, Float )
    }


type alias IndexedFaces a =
    { maxIndex : Int
    , indexMap : Array (List Int)
    , vertices : List a
    , faceIndices : List ( Int, Int, Int )
    , deferredFaces : List DeferredFace
    }


{-| Like AddIndexedTriangles, but also carries a smoothing group and collects
faces that lack normals into a `DeferredFace` list rather than failing.
Used by both `addTexturedFaces` and `addBumpyFaces`.
-}
type alias AddIndexedFaces a =
    Int
    -> Int
    -> List Vertex
    -> List FaceElement
    -> Int
    -> Array (List Int)
    -> List a
    -> List Int
    -> List ( Int, Int, Int )
    -> List DeferredFace
    -> Result String (IndexedFaces a)


{-| A face element that could not be processed by the fast path because one or
more of its vertices lack an explicit normal index. Stored together with its
source line number and smoothing group so the normal generation pass can compute
and assign the correct normal.
-}
type alias DeferredFace =
    { lineno : Int, smoothingGroup : Int, vertices : List Vertex }


{-| Shared group-level driver for `faces`, `texturedFaces`, and `bumpyFaces`
passes.

Iterates over every `Group` in turn, dispatching each group's face elements to
`add`. Faces whose vertices all have explicit normals are handled immediately by
the fast path; faces missing a normal are appended to `pending` for the
reconstruction pass.

When `pending` is empty on return the caller can build the final mesh directly
from `faceIndices` and `vertices` without any reconstruction work.

-}
triangularMesh :
    AddIndexedFaces a
    -> List Group
    -> Int
    -> Array (List Int)
    -> List a
    -> List ( Int, Int, Int )
    -> List DeferredFace
    -> Result String (IndexedFaces a)
triangularMesh add groups maxIndex indexMap vertices faceIndices pending =
    case groups of
        (Group record ((FaceElement lineno hasNormals elementVertices) :: faceElements) _ _) :: remainingGroups ->
            let
                ( firstVerts, firstPending ) =
                    if hasNormals then
                        ( elementVertices, pending )

                    else
                        ( [], { lineno = lineno, smoothingGroup = record.smoothingGroup, vertices = elementVertices } :: pending )
            in
            case add record.smoothingGroup lineno firstVerts faceElements maxIndex indexMap vertices [] faceIndices firstPending of
                Ok newState ->
                    triangularMesh add remainingGroups newState.maxIndex newState.indexMap newState.vertices newState.faceIndices newState.deferredFaces

                Err e ->
                    Err e

        (Group _ [] _ _) :: remainingGroups ->
            triangularMesh add remainingGroups maxIndex indexMap vertices faceIndices pending

        [] ->
            Ok { maxIndex = maxIndex, indexMap = indexMap, vertices = vertices, faceIndices = faceIndices, deferredFaces = pending }


{-| Face- and vertex-level inner loop for the unified faces pass.
Parallel to `addBumpyFaces`; see that function's documentation for the
fast-path and pending-path behaviour.

Note: the dedup key is `(n)` alone via `lookup1` (no UV component), and the
indexMap stores `(n, vertexIdx)` pairs rather than triples.

-}
addFaces : Frame3d Meters coordinates { defines : ObjCoordinates } -> VertexData -> AddIndexedFaces (Face coordinates)
addFaces frame vertexData sg lineno elementVertices elements maxIndex indexMap vertices indices faceIndices pending =
    case elementVertices of
        { p, n } :: remainingVertices ->
            let
                lookupArray =
                    case Array.get p indexMap of
                        Just arr ->
                            arr

                        Nothing ->
                            []

                idx =
                    lookup1 n lookupArray
            in
            if idx > -1 then
                addFaces frame
                    vertexData
                    sg
                    lineno
                    remainingVertices
                    elements
                    maxIndex
                    indexMap
                    vertices
                    (idx :: indices)
                    faceIndices
                    pending

            else
                case Array.get p vertexData.positions of
                    Just position ->
                        case Array.get n vertexData.normals of
                            Just normal ->
                                addFaces frame
                                    vertexData
                                    sg
                                    lineno
                                    remainingVertices
                                    elements
                                    (maxIndex + 1)
                                    (Array.set p (n :: maxIndex + 1 :: lookupArray) indexMap)
                                    ({ position = Point3d.placeIn frame position
                                     , normal = Direction3d.toVector (Direction3d.placeIn frame normal)
                                     }
                                        :: vertices
                                    )
                                    (maxIndex + 1 :: indices)
                                    faceIndices
                                    pending

                            Nothing ->
                                formatError lineno "Index out of range"

                    Nothing ->
                        formatError lineno "Index out of range"

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
                (FaceElement newLineno hasNormals newVerts) :: rest ->
                    if hasNormals then
                        addFaces frame vertexData sg newLineno newVerts rest maxIndex indexMap vertices [] newFaceIndices pending

                    else
                        addFaces frame
                            vertexData
                            sg
                            newLineno
                            []
                            rest
                            maxIndex
                            indexMap
                            vertices
                            []
                            newFaceIndices
                            ({ lineno = newLineno, smoothingGroup = sg, vertices = newVerts } :: pending)

                [] ->
                    Ok { maxIndex = maxIndex, indexMap = indexMap, vertices = vertices, faceIndices = newFaceIndices, deferredFaces = pending }


{-| Face- and vertex-level inner loop for the unified textured-faces pass.
Parallel to `addBumpyFaces`; see that function's documentation for the
fast-path and pending-path behaviour.
-}
addTexturedFaces : Frame3d Meters coordinates { defines : ObjCoordinates } -> VertexData -> AddIndexedFaces (TexturedFace coordinates)
addTexturedFaces frame vertexData sg lineno elementVertices elements maxIndex indexMap vertices indices faceIndices pending =
    case elementVertices of
        { p, uv, n } :: remainingVertices ->
            let
                lookupArray =
                    case Array.get p indexMap of
                        Just arr ->
                            arr

                        Nothing ->
                            []

                idx =
                    lookup2 uv n lookupArray
            in
            if idx > -1 then
                addTexturedFaces frame
                    vertexData
                    sg
                    lineno
                    remainingVertices
                    elements
                    maxIndex
                    indexMap
                    vertices
                    (idx :: indices)
                    faceIndices
                    pending

            else
                case Array.get p vertexData.positions of
                    Just position ->
                        case Array.get n vertexData.normals of
                            Just normal ->
                                case Array.get uv vertexData.uvs of
                                    Just uvCoord ->
                                        addTexturedFaces frame
                                            vertexData
                                            sg
                                            lineno
                                            remainingVertices
                                            elements
                                            (maxIndex + 1)
                                            (Array.set p (uv :: n :: maxIndex + 1 :: lookupArray) indexMap)
                                            ({ position = Point3d.placeIn frame position
                                             , normal = Direction3d.toVector (Direction3d.placeIn frame normal)
                                             , uv = uvCoord
                                             }
                                                :: vertices
                                            )
                                            (maxIndex + 1 :: indices)
                                            faceIndices
                                            pending

                                    Nothing ->
                                        formatError lineno "Index out of range"

                            Nothing ->
                                formatError lineno "Index out of range"

                    Nothing ->
                        formatError lineno "Index out of range"

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
                (FaceElement newLineno hasNormals newVerts) :: rest ->
                    if hasNormals then
                        addTexturedFaces frame vertexData sg newLineno newVerts rest maxIndex indexMap vertices [] newFaceIndices pending

                    else
                        addTexturedFaces frame
                            vertexData
                            sg
                            newLineno
                            []
                            rest
                            maxIndex
                            indexMap
                            vertices
                            []
                            newFaceIndices
                            ({ lineno = newLineno, smoothingGroup = sg, vertices = newVerts } :: pending)

                [] ->
                    Ok { maxIndex = maxIndex, indexMap = indexMap, vertices = vertices, faceIndices = newFaceIndices, deferredFaces = pending }


{-| Vertices are accumulated in reverse order.
-}
generateFacesNormals :
    Frame3d Meters coordinates { defines : ObjCoordinates }
    -> VertexData
    -> List DeferredFace
    -> Dict Int (List Int)
    -> List (Face coordinates)
    -> Int
    -> List ( Int, Int, Int )
    -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
    -> Bool
    -> ( List (Face coordinates), List ( Int, Int, Int ) )
generateFacesNormals frame vertexData pendingFaces vMap verts idx faceAcc weightedNormals bitflags =
    case pendingFaces of
        [] ->
            ( verts, faceAcc )

        pf :: rest ->
            if pf.smoothingGroup == 0 then
                -- Flat shading: unshared vertices per face
                case pf.vertices of
                    v0 :: v1 :: restVerts ->
                        case Array.get v0.p vertexData.positions of
                            Just pos0 ->
                                case Array.get v1.p vertexData.positions of
                                    Just pos1 ->
                                        let
                                            pos0InFrame =
                                                Point3d.placeIn frame pos0

                                            pos1InFrame =
                                                Point3d.placeIn frame pos1
                                        in
                                        flatFacesNormals restVerts rest frame vertexData pos0InFrame pos1InFrame False Vector3d.zero vMap verts idx faceAcc weightedNormals bitflags

                                    Nothing ->
                                        generateFacesNormals frame vertexData rest vMap verts idx faceAcc weightedNormals bitflags

                            Nothing ->
                                generateFacesNormals frame vertexData rest vMap verts idx faceAcc weightedNormals bitflags

                    _ ->
                        generateFacesNormals frame vertexData rest vMap verts idx faceAcc weightedNormals bitflags

            else
                buildFaceVertices frame vertexData rest weightedNormals bitflags pf.smoothingGroup pf.vertices vMap verts idx [] faceAcc


buildFaceVertices :
    Frame3d Meters coordinates { defines : ObjCoordinates }
    -> VertexData
    -> List DeferredFace
    -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
    -> Bool
    -> Int
    -> List Vertex
    -> Dict Int (List Int)
    -> List (Face coordinates)
    -> Int
    -> List Int
    -> List ( Int, Int, Int )
    -> ( List (Face coordinates), List ( Int, Int, Int ) )
buildFaceVertices frame vertexData rest weightedNormals bitflags sg vertices vMap verts idx faceIndices faceAcc =
    case vertices of
        [] ->
            case faceIndices of
                i1 :: restIndices ->
                    generateFacesNormals frame vertexData rest vMap verts idx (groupIndices i1 restIndices faceAcc) weightedNormals bitflags

                [] ->
                    generateFacesNormals frame vertexData rest vMap verts idx faceAcc weightedNormals bitflags

        { p } :: restVerts ->
            let
                existingIdx =
                    case Dict.get p vMap of
                        Just entries ->
                            lookup1 sg entries

                        Nothing ->
                            -1
            in
            if existingIdx > -1 then
                buildFaceVertices frame vertexData rest weightedNormals bitflags sg restVerts vMap verts idx (existingIdx :: faceIndices) faceAcc

            else
                case lookupNormal bitflags p sg weightedNormals of
                    Just normal ->
                        case Array.get p vertexData.positions of
                            Just position ->
                                buildFaceVertices frame
                                    vertexData
                                    rest
                                    weightedNormals
                                    bitflags
                                    sg
                                    restVerts
                                    (Dict.insert p
                                        -- (p, sg) is guaranteed unique here (lookup1 returned -1 above),
                                        -- so we always prepend a fresh pair; never overwrite an existing entry.
                                        (case Dict.get p vMap of
                                            Just vmEntries ->
                                                sg :: idx :: vmEntries

                                            Nothing ->
                                                [ sg, idx ]
                                        )
                                        vMap
                                    )
                                    ({ position = Point3d.placeIn frame position

                                     -- Normalize the area-weighted sum to get a unit normal.
                                     , normal = Vector3d.placeIn frame (Vector3d.normalize normal)
                                     }
                                        :: verts
                                    )
                                    (idx + 1)
                                    (idx :: faceIndices)
                                    faceAcc

                            Nothing ->
                                buildFaceVertices frame vertexData rest weightedNormals bitflags sg restVerts vMap verts idx faceIndices faceAcc

                    Nothing ->
                        buildFaceVertices frame vertexData rest weightedNormals bitflags sg restVerts vMap verts idx faceIndices faceAcc


{-| Vertices are accumulated in reverse order.
-}
flatFacesNormals :
    List Vertex
    -> List DeferredFace
    -> Frame3d Meters coordinates { defines : ObjCoordinates }
    -> VertexData
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> Bool
    -> Vector3d Unitless coordinates
    -> Dict Int (List Int)
    -> List (Face coordinates)
    -> Int
    -> List ( Int, Int, Int )
    -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
    -> Bool
    -> ( List (Face coordinates), List ( Int, Int, Int ) )
flatFacesNormals verts rest frame vertexData pos0InFrame prevPosInFrame normalFound normalInFrame vMap accVerts accIdx accFaceAcc weightedNormals bitflags =
    case verts of
        [] ->
            generateFacesNormals frame vertexData rest vMap accVerts accIdx accFaceAcc weightedNormals bitflags

        vN :: restVerts ->
            case Array.get vN.p vertexData.positions of
                Just posN ->
                    let
                        posNInFrame =
                            Point3d.placeIn frame posN

                        normal =
                            if normalFound then
                                normalInFrame

                            else
                                Vector3d.normalize
                                    (Vector3d.cross
                                        (Vector3d.from pos0InFrame prevPosInFrame)
                                        (Vector3d.from pos0InFrame posNInFrame)
                                    )
                    in
                    flatFacesNormals restVerts
                        rest
                        frame
                        vertexData
                        posNInFrame
                        pos0InFrame
                        True
                        normal
                        vMap
                        ({ position = pos0InFrame, normal = normal }
                            :: { position = prevPosInFrame, normal = normal }
                            :: { position = posNInFrame, normal = normal }
                            :: accVerts
                        )
                        (accIdx + 3)
                        (( accIdx, accIdx + 1, accIdx + 2 ) :: accFaceAcc)
                        weightedNormals
                        bitflags

                Nothing ->
                    flatFacesNormals restVerts rest frame vertexData pos0InFrame prevPosInFrame normalFound normalInFrame vMap accVerts accIdx accFaceAcc weightedNormals bitflags


{-| Vertices are accumulated in reverse order.
-}
generateTexturedFacesNormals :
    Frame3d Meters coordinates { defines : ObjCoordinates }
    -> VertexData
    -> List DeferredFace
    -> Dict Int (List Int)
    -> List (TexturedFace coordinates)
    -> Int
    -> List ( Int, Int, Int )
    -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
    -> Bool
    -> ( List (TexturedFace coordinates), List ( Int, Int, Int ) )
generateTexturedFacesNormals frame vertexData pendingFaces vMap verts idx faceAcc weightedNormals bitflags =
    case pendingFaces of
        [] ->
            ( verts, faceAcc )

        pf :: rest ->
            if pf.smoothingGroup == 0 then
                -- Flat shading: unshared vertices per face
                case pf.vertices of
                    v0 :: v1 :: restVerts ->
                        case Array.get v0.p vertexData.positions of
                            Just pos0 ->
                                case Array.get v1.p vertexData.positions of
                                    Just pos1 ->
                                        case Array.get v0.uv vertexData.uvs of
                                            Just uv0 ->
                                                case Array.get v1.uv vertexData.uvs of
                                                    Just uv1 ->
                                                        let
                                                            pos0InFrame =
                                                                Point3d.placeIn frame pos0

                                                            pos1InFrame =
                                                                Point3d.placeIn frame pos1
                                                        in
                                                        flatTexturedFacesNormals restVerts rest frame vertexData pos0InFrame pos1InFrame uv0 uv1 False Vector3d.zero vMap verts idx faceAcc weightedNormals bitflags

                                                    Nothing ->
                                                        generateTexturedFacesNormals frame vertexData rest vMap verts idx faceAcc weightedNormals bitflags

                                            Nothing ->
                                                generateTexturedFacesNormals frame vertexData rest vMap verts idx faceAcc weightedNormals bitflags

                                    Nothing ->
                                        generateTexturedFacesNormals frame vertexData rest vMap verts idx faceAcc weightedNormals bitflags

                            Nothing ->
                                generateTexturedFacesNormals frame vertexData rest vMap verts idx faceAcc weightedNormals bitflags

                    _ ->
                        generateTexturedFacesNormals frame vertexData rest vMap verts idx faceAcc weightedNormals bitflags

            else
                buildTexturedFaceVertices frame vertexData rest weightedNormals bitflags pf.smoothingGroup pf.vertices vMap verts idx [] faceAcc


buildTexturedFaceVertices :
    Frame3d Meters coordinates { defines : ObjCoordinates }
    -> VertexData
    -> List DeferredFace
    -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
    -> Bool
    -> Int
    -> List Vertex
    -> Dict Int (List Int)
    -> List (TexturedFace coordinates)
    -> Int
    -> List Int
    -> List ( Int, Int, Int )
    -> ( List (TexturedFace coordinates), List ( Int, Int, Int ) )
buildTexturedFaceVertices frame vertexData rest weightedNormals bitflags sg vertices vMap verts idx faceIndices faceAcc =
    case vertices of
        [] ->
            case faceIndices of
                i1 :: restIndices ->
                    generateTexturedFacesNormals frame vertexData rest vMap verts idx (groupIndices i1 restIndices faceAcc) weightedNormals bitflags

                [] ->
                    generateTexturedFacesNormals frame vertexData rest vMap verts idx faceAcc weightedNormals bitflags

        { p, uv } :: restVerts ->
            let
                existingIdx =
                    case Dict.get p vMap of
                        Just entries ->
                            lookup2 sg uv entries

                        Nothing ->
                            -1
            in
            if existingIdx > -1 then
                buildTexturedFaceVertices frame vertexData rest weightedNormals bitflags sg restVerts vMap verts idx (existingIdx :: faceIndices) faceAcc

            else
                case lookupNormal bitflags p sg weightedNormals of
                    Just normal ->
                        case Array.get uv vertexData.uvs of
                            Just uvCoord ->
                                case Array.get p vertexData.positions of
                                    Just position ->
                                        buildTexturedFaceVertices frame
                                            vertexData
                                            rest
                                            weightedNormals
                                            bitflags
                                            sg
                                            restVerts
                                            (Dict.insert p
                                                -- (sg, uv) is guaranteed unique here (lookup2 returned -1 above),
                                                -- so we always prepend a fresh triple; never overwrite an existing entry.
                                                (case Dict.get p vMap of
                                                    Just entries ->
                                                        sg :: uv :: idx :: entries

                                                    Nothing ->
                                                        [ sg, uv, idx ]
                                                )
                                                vMap
                                            )
                                            ({ position = Point3d.placeIn frame position

                                             -- Normalize the area-weighted sum to get a unit normal.
                                             , normal = Vector3d.placeIn frame (Vector3d.normalize normal)
                                             , uv = uvCoord
                                             }
                                                :: verts
                                            )
                                            (idx + 1)
                                            (idx :: faceIndices)
                                            faceAcc

                                    Nothing ->
                                        buildTexturedFaceVertices frame vertexData rest weightedNormals bitflags sg restVerts vMap verts idx faceIndices faceAcc

                            Nothing ->
                                buildTexturedFaceVertices frame vertexData rest weightedNormals bitflags sg restVerts vMap verts idx faceIndices faceAcc

                    Nothing ->
                        buildTexturedFaceVertices frame vertexData rest weightedNormals bitflags sg restVerts vMap verts idx faceIndices faceAcc


{-| Vertices are accumulated in reverse order.
-}
flatTexturedFacesNormals :
    List Vertex
    -> List DeferredFace
    -> Frame3d Meters coordinates { defines : ObjCoordinates }
    -> VertexData
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> ( Float, Float )
    -> ( Float, Float )
    -> Bool
    -> Vector3d Unitless coordinates
    -> Dict Int (List Int)
    -> List (TexturedFace coordinates)
    -> Int
    -> List ( Int, Int, Int )
    -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
    -> Bool
    -> ( List (TexturedFace coordinates), List ( Int, Int, Int ) )
flatTexturedFacesNormals verts rest frame vertexData pos0InFrame prevPosInFrame uv0 prevUV normalFound normalInFrame vMap accVerts accIdx accFaceAcc weightedNormals bitflags =
    case verts of
        [] ->
            generateTexturedFacesNormals frame vertexData rest vMap accVerts accIdx accFaceAcc weightedNormals bitflags

        vN :: restVerts ->
            case Array.get vN.p vertexData.positions of
                Just posN ->
                    case Array.get vN.uv vertexData.uvs of
                        Just uvN ->
                            let
                                posNInFrame =
                                    Point3d.placeIn frame posN

                                normal =
                                    if normalFound then
                                        normalInFrame

                                    else
                                        Vector3d.normalize
                                            (Vector3d.cross
                                                (Vector3d.from pos0InFrame prevPosInFrame)
                                                (Vector3d.from pos0InFrame posNInFrame)
                                            )
                            in
                            flatTexturedFacesNormals restVerts
                                rest
                                frame
                                vertexData
                                pos0InFrame
                                posNInFrame
                                uv0
                                uvN
                                True
                                normal
                                vMap
                                ({ position = pos0InFrame, normal = normal, uv = uv0 }
                                    :: { position = prevPosInFrame, normal = normal, uv = prevUV }
                                    :: { position = posNInFrame, normal = normal, uv = uvN }
                                    :: accVerts
                                )
                                (accIdx + 3)
                                (( accIdx, accIdx + 1, accIdx + 2 ) :: accFaceAcc)
                                weightedNormals
                                bitflags

                        Nothing ->
                            flatTexturedFacesNormals restVerts rest frame vertexData pos0InFrame prevPosInFrame uv0 prevUV normalFound normalInFrame vMap accVerts accIdx accFaceAcc weightedNormals bitflags

                Nothing ->
                    flatTexturedFacesNormals restVerts rest frame vertexData pos0InFrame prevPosInFrame uv0 prevUV normalFound normalInFrame vMap accVerts accIdx accFaceAcc weightedNormals bitflags


smoothingGroups : List Group -> Set Int
smoothingGroups groups =
    Set.fromList (smoothingGroupsHelp groups [])


smoothingGroupsMask : List Group -> Int
smoothingGroupsMask groups =
    List.foldl Bitwise.or 0 (smoothingGroupsHelp groups [])


smoothingGroupsHelp : List Group -> List Int -> List Int
smoothingGroupsHelp groups acc =
    case groups of
        [] ->
            acc

        (Group record _ _ _) :: rest ->
            if record.smoothingGroup == 0 then
                smoothingGroupsHelp rest acc

            else
                smoothingGroupsHelp rest (record.smoothingGroup :: acc)


{-| Accumulate area-weighted normals for the smoothing-group reconstruction pass.

Iterates over `vertexData.fullGroups`, skipping groups whose smoothing-group ID
is not in `smoothingIds`. For each qualifying face the area-weighted face normal
(cross product, not yet normalised) is added to every corner's bucket.

The `keyFn` argument maps a `Vertex` and its smoothing-group ID to the
dictionary key for that corner. Callers pass:

    - `\v sg -> ( v.p, sg )` for `faces` (no UV component in key)
    - `\v sg -> ( v.p, sg, v.uv )` for `texturedFaces` / `bumpyFaces`

-}
collectWeightedNormals : VertexData -> List Group -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates )) -> Set Int -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
collectWeightedNormals vertexData groups acc filteredSmoothingGroups =
    collectWeightedNormalsHelp (\sg -> Set.member sg filteredSmoothingGroups) vertexData groups acc


collectBitflagWeightedNormals : VertexData -> List Group -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates )) -> Int -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
collectBitflagWeightedNormals vertexData groups acc allBits =
    collectWeightedNormalsHelp (\sg -> Bitwise.and sg allBits /= 0) vertexData groups acc


collectWeightedNormalsHelp : (Int -> Bool) -> VertexData -> List Group -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates )) -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
collectWeightedNormalsHelp matches vertexData groups acc =
    case groups of
        [] ->
            acc

        (Group { smoothingGroup } faceElements _ _) :: rest ->
            if matches smoothingGroup then
                collectWeightedNormalsHelp matches
                    vertexData
                    rest
                    (collectWeightedNormalsFaces vertexData smoothingGroup faceElements acc)

            else
                collectWeightedNormalsHelp matches vertexData rest acc


{-| Sum the area-weighted cross products of all fan triangles for a polygon.

For a triangle this is a single cross product; for a quad it is the sum of two,
and so on. Summing (rather than taking just the first triangle) is necessary so
that the accumulated magnitude at each vertex reflects the _full_ polygon area.
Without this, a quad face would contribute only ~half its area weight compared
to an adjacent triangle face, biasing the smooth normal toward the triangle.

Vertices whose position index is out of range are simply skipped; the remaining
triangles still contribute their area.

-}
polygonFanNormal :
    VertexData
    -> { x : Float, y : Float, z : Float }
    -> List Vertex
    -> Vector3d Unitless ObjCoordinates
    -> Vector3d Unitless ObjCoordinates
polygonFanNormal vertexData p0 vertices acc =
    case vertices of
        vA :: ((vB :: _) as rest) ->
            case Array.get vA.p vertexData.positions of
                Just posA ->
                    case Array.get vB.p vertexData.positions of
                        Just posB ->
                            let
                                pA =
                                    Point3d.toMeters posA

                                pB =
                                    Point3d.toMeters posB

                                ax =
                                    pA.x - p0.x

                                ay =
                                    pA.y - p0.y

                                az =
                                    pA.z - p0.z

                                bx =
                                    pB.x - p0.x

                                by =
                                    pB.y - p0.y

                                bz =
                                    pB.z - p0.z
                            in
                            polygonFanNormal vertexData
                                p0
                                rest
                                (Vector3d.plus acc
                                    (Vector3d.unitless
                                        (by * az - bz * ay)
                                        (bz * ax - bx * az)
                                        (bx * ay - by * ax)
                                    )
                                )

                        Nothing ->
                            polygonFanNormal vertexData p0 rest acc

                Nothing ->
                    polygonFanNormal vertexData p0 rest acc

        _ ->
            acc


collectWeightedNormalsFaces : VertexData -> Int -> List FaceElement -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates )) -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
collectWeightedNormalsFaces vertexData sg faceElements acc =
    case faceElements of
        [] ->
            acc

        (FaceElement _ _ ((v0 :: _ :: _ :: _) as vertices)) :: rest ->
            case Array.get v0.p vertexData.positions of
                Just pos0 ->
                    let
                        normal =
                            polygonFanNormal vertexData (Point3d.toMeters pos0) (List.drop 1 vertices) Vector3d.zero
                    in
                    collectWeightedNormalsFaces vertexData
                        sg
                        rest
                        (collectWeightedNormalsVertices sg normal vertices acc)

                Nothing ->
                    collectWeightedNormalsFaces vertexData sg rest acc

        (FaceElement _ _ _) :: rest ->
            collectWeightedNormalsFaces vertexData sg rest acc


lookupNormal : Bool -> Int -> Int -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates )) -> Maybe (Vector3d Unitless ObjCoordinates)
lookupNormal bitflags p sg weightedNormals =
    case Dict.get p weightedNormals of
        Nothing ->
            Nothing

        Just entries ->
            lookupNormalHelp bitflags sg entries Nothing


lookupNormalHelp : Bool -> Int -> List ( Int, Vector3d Unitless ObjCoordinates ) -> Maybe (Vector3d Unitless ObjCoordinates) -> Maybe (Vector3d Unitless ObjCoordinates)
lookupNormalHelp bitflags sg entries acc =
    case entries of
        [] ->
            acc

        ( s, v ) :: rest ->
            let
                matches =
                    if bitflags then
                        Bitwise.and sg s /= 0

                    else
                        sg == s
            in
            lookupNormalHelp bitflags
                sg
                rest
                (if matches then
                    case acc of
                        Nothing ->
                            Just v

                        Just existing ->
                            Just (Vector3d.plus existing v)

                 else
                    acc
                )


{-| Add `normal` to the entry for `sg` in `remaining`, or prepend a new entry if none exists.
Order within the list is not significant; `lookupNormal` scans the whole list.
-}
addNormal : Int -> Vector3d Unitless ObjCoordinates -> List ( Int, Vector3d Unitless ObjCoordinates ) -> List ( Int, Vector3d Unitless ObjCoordinates ) -> List ( Int, Vector3d Unitless ObjCoordinates )
addNormal sg normal remaining prefix =
    case remaining of
        [] ->
            ( sg, normal ) :: prefix

        ( s, v ) :: rest ->
            if sg - s == 0 then
                -- The cross product magnitude is proportional to triangle area,
                -- so larger triangles contribute more weight to the smoothed normal.
                -- Fold the existing vector into normal and drop the old entry from its position.
                addNormal sg (Vector3d.plus normal v) rest prefix

            else
                addNormal sg normal rest (( s, v ) :: prefix)


collectWeightedNormalsVertices : Int -> Vector3d Unitless ObjCoordinates -> List Vertex -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates )) -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
collectWeightedNormalsVertices sg normal vertices acc =
    case vertices of
        [] ->
            acc

        v :: rest ->
            -- Key by position: v.p alone is sufficient when each position belongs to one group,
            -- but the same geometric position can appear in multiple smoothing groups and each
            -- group needs its own normal, so sg is stored in the inner list.
            collectWeightedNormalsVertices sg
                normal
                rest
                (Dict.insert v.p
                    (case Dict.get v.p acc of
                        Just entries ->
                            addNormal sg normal entries []

                        Nothing ->
                            [ ( sg, normal ) ]
                    )
                    acc
                )


{-| Adds tangents to the vertices, initializing them to zero. This is necessary for the tangent accumulation.
Note: this reverses the order of vertices.
-}
addZeroTangents :
    List (TexturedFace coordinates)
    -> List { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ), tangent : Vector3d Unitless coordinates, bitangent : Vector3d Unitless coordinates }
    -> List { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ), tangent : Vector3d Unitless coordinates, bitangent : Vector3d Unitless coordinates }
addZeroTangents list acc =
    case list of
        [] ->
            acc

        v :: rest ->
            addZeroTangents rest
                ({ position = v.position
                 , normal = v.normal
                 , uv = v.uv
                 , tangent = Vector3d.zero
                 , bitangent = Vector3d.zero
                 }
                    :: acc
                )


computeTangents :
    List String
    -> List (TexturedFace coordinates)
    -> List ( Int, Int, Int )
    -> Result String (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ), tangent : Vector3d Unitless coordinates, tangentBasisIsRightHanded : Bool })
computeTangents filters reversedTexturedVertices faceIndices =
    let
        vertices =
            Array.fromList (addZeroTangents reversedTexturedVertices [])

        orthogonalizedVertices =
            Array.map
                (\v ->
                    let
                        (Quantity dot) =
                            Vector3d.dot v.tangent v.normal

                        -- Gram-Schmidt: subtract the component of the accumulated
                        -- tangent that points along the normal (T - (N·T)N),
                        -- leaving a tangent that lies flat on the surface,
                        -- then normalize to unit length.
                        tangent =
                            Vector3d.normalize (Vector3d.minus (Vector3d.scaleBy dot v.normal) v.tangent)

                        (Quantity handednessDot) =
                            Vector3d.dot (Vector3d.cross v.normal v.tangent) v.bitangent
                    in
                    { position = v.position
                    , uv = v.uv
                    , tangent = tangent
                    , normal = v.normal
                    , tangentBasisIsRightHanded = handednessDot > 0
                    }
                )
                (computeTangentsHelp faceIndices vertices)
    in
    buildMeshResult filters orthogonalizedVertices faceIndices


computeTangentsHelp :
    List ( Int, Int, Int )
    ->
        Array
            { normal : Vector3d Unitless coordinates
            , position : Point3d Meters coordinates
            , uv : ( Float, Float )
            , tangent : Vector3d Unitless coordinates
            , bitangent : Vector3d Unitless coordinates
            }
    ->
        Array
            { normal : Vector3d Unitless coordinates
            , position : Point3d Meters coordinates
            , uv : ( Float, Float )
            , tangent : Vector3d Unitless coordinates
            , bitangent : Vector3d Unitless coordinates
            }
computeTangentsHelp faceIndices vertices =
    case faceIndices of
        [] ->
            vertices

        ( i1, i2, i3 ) :: rest ->
            case Array.get i1 vertices of
                Just vertex1 ->
                    case Array.get i2 vertices of
                        Just vertex2 ->
                            case Array.get i3 vertices of
                                Just vertex3 ->
                                    let
                                        p1 =
                                            Point3d.toMeters vertex1.position

                                        p2 =
                                            Point3d.toMeters vertex2.position

                                        p3 =
                                            Point3d.toMeters vertex3.position

                                        ( u1, v1 ) =
                                            vertex1.uv

                                        ( u2, v2 ) =
                                            vertex2.uv

                                        ( u3, v3 ) =
                                            vertex3.uv

                                        dX1 =
                                            p2.x - p1.x

                                        dX2 =
                                            p3.x - p1.x

                                        dY1 =
                                            p2.y - p1.y

                                        dY2 =
                                            p3.y - p1.y

                                        dZ1 =
                                            p2.z - p1.z

                                        dZ2 =
                                            p3.z - p1.z

                                        dU1 =
                                            u2 - u1

                                        dU2 =
                                            u3 - u1

                                        dV1 =
                                            v2 - v1

                                        dV2 =
                                            v3 - v1

                                        r =
                                            1.0 / (dU1 * dV2 - dV1 * dU2)

                                        tangent =
                                            Vector3d.unitless
                                                ((dX1 * dV2 - dX2 * dV1) * r)
                                                ((dY1 * dV2 - dY2 * dV1) * r)
                                                ((dZ1 * dV2 - dZ2 * dV1) * r)

                                        bitangent =
                                            Vector3d.unitless
                                                ((dX2 * dU1 - dX1 * dU2) * r)
                                                ((dY2 * dU1 - dY1 * dU2) * r)
                                                ((dZ2 * dU1 - dZ1 * dU2) * r)
                                    in
                                    computeTangentsHelp rest
                                        (Array.set i3
                                            { normal = vertex3.normal
                                            , position = vertex3.position
                                            , uv = vertex3.uv
                                            , tangent = Vector3d.plus tangent vertex3.tangent
                                            , bitangent = Vector3d.plus bitangent vertex3.bitangent
                                            }
                                            (Array.set i2
                                                { normal = vertex2.normal
                                                , position = vertex2.position
                                                , uv = vertex2.uv
                                                , tangent = Vector3d.plus tangent vertex2.tangent
                                                , bitangent = Vector3d.plus bitangent vertex2.bitangent
                                                }
                                                (Array.set i1
                                                    { normal = vertex1.normal
                                                    , position = vertex1.position
                                                    , uv = vertex1.uv
                                                    , tangent = Vector3d.plus tangent vertex1.tangent
                                                    , bitangent = Vector3d.plus bitangent vertex1.bitangent
                                                    }
                                                    vertices
                                                )
                                            )
                                        )

                                Nothing ->
                                    computeTangentsHelp rest vertices

                        Nothing ->
                            computeTangentsHelp rest vertices

                Nothing ->
                    computeTangentsHelp rest vertices
