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


{-| Pipeline for faces / texturedFaces:

Fast path (all faces have explicit normals in OBJ):

      triangularMesh / addFaces
        outFaceVertices: accumulated by prepending         → [vN,..,v1] (reversed)
        outFaceIndices:  two reversals restore parse order → [f1,..,fN] (parse order)

      Array.fromList (List.reverse outFaceVertices) → [v1,..,vN] ✓
      outFaceIndices passed as-is                   → [f1,..,fN] ✓

Deferred path (some faces need normals reconstructed):

      triangularMesh / addFaces
        outFaceVertices: [vM,..,v1]   (reversed)
        outFaceIndices:  [f1,..,fM]   (parse order, fast-path faces only)
        outPendingFaces: [fd1,..,fdK] (parse order)

      generateFacesNormals (prepends new outFaceVertices/outFaceIndices onto existing state)
        outFaceVertices: [vN,..,vM+1 | vM,..,v1] (still reversed)
        outFaceIndices:  [fdK,..,fd1 | f1,..,fM] (reversed: reconstructed indices prepended onto fast-path indices)

      Array.fromList (List.reverse outFaceVertices) → [v1,..,vN]             ✓
      List.reverse outFaceIndices                   → [fM,..,f1, fd1,..,fdK] ✓

-}
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
            case state.pendingFaces of
                [] ->
                    buildMeshResult filters (Array.fromList (List.reverse state.faceVertices)) state.faceIndices

                _ ->
                    let
                        weightedNormals =
                            if bitflags then
                                collectBitflagWeightedNormals vertexData filteredGroups Dict.empty (smoothingGroupsMask filteredGroups)

                            else
                                collectWeightedNormals vertexData filteredGroups Dict.empty (smoothingGroupsSet filteredGroups)

                        ( outFaceVertices, outFaceIndices ) =
                            generateFacesNormals frame vertexData state.pendingFaces Dict.empty state.faceVertices (state.maxIndex + 1) state.faceIndices weightedNormals bitflags
                    in
                    buildMeshResult filters (Array.fromList (List.reverse outFaceVertices)) (List.reverse outFaceIndices)


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
            case state.pendingFaces of
                [] ->
                    buildMeshResult filters (Array.fromList (List.reverse state.faceVertices)) state.faceIndices

                _ ->
                    let
                        weightedNormals =
                            if bitflags then
                                collectBitflagWeightedNormals vertexData filteredGroups Dict.empty (smoothingGroupsMask filteredGroups)

                            else
                                collectWeightedNormals vertexData filteredGroups Dict.empty (smoothingGroupsSet filteredGroups)

                        ( outFaceVertices, outFaceIndices ) =
                            generateTexturedFacesNormals frame vertexData state.pendingFaces Dict.empty state.faceVertices (state.maxIndex + 1) state.faceIndices weightedNormals bitflags
                    in
                    buildMeshResult filters (Array.fromList (List.reverse outFaceVertices)) (List.reverse outFaceIndices)


{-| Pipeline for bumpyFaces:

Fast path (all faces have explicit normals in OBJ):

      triangularMesh / addTexturedFaces
        outFaceVertices: accumulated by prepending         → [vN,..,v1] (reversed)
        outFaceIndices:  two reversals restore parse order → [f1,..,fN] (parse order)

      computeTangents filters outFaceVertices outFaceIndices
        outFaceVertices received reversed → addZeroTangents un-reverses → [v1,..,vN] ✓
        outFaceIndices passed as-is                                     → [f1,..,fN] ✓

Deferred path (some faces need normals reconstructed):

      triangularMesh / addTexturedFaces
        outFaceVertices: [vM,..,v1]   (reversed)
        outFaceIndices:  [f1,..,fM]   (parse order, fast-path faces only)
        outPendingFaces: [fd1,..,fdK] (parse order)

      generateTexturedFacesNormals (prepends new outFaceVertices/outFaceIndices onto existing state)
        outFaceVertices: [vN,..,vM+1 | vM,..,v1]  (still reversed)
        outFaceIndices:  [fdK,..,fd1 | f1,..,fM]  (reversed: reconstructed indices prepended onto fast-path indices)

      computeTangents filters outFaceVertices (List.reverse outFaceIndices)
        outFaceVertices received reversed → addZeroTangents un-reverses → [v1,..,vN]             ✓
        List.reverse outFaceIndices                                     → [fM,..,f1, fd1,..,fdK] ✓

-}
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
            case state.pendingFaces of
                [] ->
                    computeTangents filters state.faceVertices state.faceIndices

                _ ->
                    let
                        weightedNormals =
                            if bitflags then
                                collectBitflagWeightedNormals vertexData filteredGroups Dict.empty (smoothingGroupsMask filteredGroups)

                            else
                                collectWeightedNormals vertexData filteredGroups Dict.empty (smoothingGroupsSet filteredGroups)

                        ( outFaceVertices, outFaceIndices ) =
                            generateTexturedFacesNormals frame vertexData state.pendingFaces Dict.empty state.faceVertices (state.maxIndex + 1) state.faceIndices weightedNormals bitflags
                    in
                    computeTangents filters outFaceVertices (List.reverse outFaceIndices)


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
    , faceVertices : List a
    , faceIndices : List ( Int, Int, Int )
    , pendingFaces : List PendingFace
    }


{-| Like AddIndexedTriangles, but also carries a smoothing group and collects
faces that lack normals into an `outPendingFaces` list rather than failing.
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
    -> List PendingFace
    -> Result String (IndexedFaces a)


{-| A face element that could not be processed by the fast path because one or
more of its vertices lack an explicit normal index. Stored together with its
source line number and smoothing group so the normal generation pass can compute
and assign the correct normal.
-}
type alias PendingFace =
    { lineno : Int, smoothingGroup : Int, elementVertices : List Vertex }


type alias FaceVerticesResult coordinates =
    { faceVertices : List (Face coordinates)
    , idx : Int
    , faceIndices : List ( Int, Int, Int )
    , vMap : Dict Int (List Int)
    }


{-| Shared group-level driver for `faces`, `texturedFaces`, and `bumpyFaces`
passes.

Iterates over every `Group` in turn, dispatching each group's face elements to
`add`. Faces whose vertices all have explicit normals are handled immediately by
the fast path; faces missing a normal are appended to `outPendingFaces` for the
reconstruction pass.

When `outPendingFaces` is empty on return the caller can build the final mesh directly
from `outFaceIndices` and `outFaceVertices` without any reconstruction work.

-}
triangularMesh :
    AddIndexedFaces a
    -> List Group
    -> Int
    -> Array (List Int)
    -> List a
    -> List ( Int, Int, Int )
    -> List PendingFace
    -> Result String (IndexedFaces a)
triangularMesh add groups maxIndex indexMap outFaceVertices outFaceIndices outPendingFaces =
    case groups of
        (Group record ((FaceElement lineno hasNormals elementVertices) :: faceElements) _ _) :: remainingGroups ->
            let
                ( firstElementVertices, firstPendingFace ) =
                    if hasNormals then
                        ( elementVertices, outPendingFaces )

                    else
                        ( [], { lineno = lineno, smoothingGroup = record.smoothingGroup, elementVertices = elementVertices } :: outPendingFaces )
            in
            case add record.smoothingGroup lineno firstElementVertices faceElements maxIndex indexMap outFaceVertices [] outFaceIndices firstPendingFace of
                Ok newState ->
                    triangularMesh add remainingGroups newState.maxIndex newState.indexMap newState.faceVertices newState.faceIndices newState.pendingFaces

                Err e ->
                    Err e

        (Group _ [] _ _) :: remainingGroups ->
            triangularMesh add remainingGroups maxIndex indexMap outFaceVertices outFaceIndices outPendingFaces

        [] ->
            Ok { maxIndex = maxIndex, indexMap = indexMap, faceVertices = outFaceVertices, faceIndices = outFaceIndices, pendingFaces = outPendingFaces }


{-| Face- and vertex-level inner loop for the unified faces pass.
Parallel to `addBumpyFaces`; see that function's documentation for the
fast-path and pending-path behaviour.

Note: the dedup key is `(n)` alone via `lookup1` (no UV component), and the
indexMap stores `(n, vertexIdx)` pairs rather than triples.

-}
addFaces : Frame3d Meters coordinates { defines : ObjCoordinates } -> VertexData -> AddIndexedFaces (Face coordinates)
addFaces frame vertexData smoothingGroup lineno elementVertices elements maxIndex indexMap outFaceVertices outIndices outFaceIndices outPendingFaces =
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
                    smoothingGroup
                    lineno
                    remainingVertices
                    elements
                    maxIndex
                    indexMap
                    outFaceVertices
                    (idx :: outIndices)
                    outFaceIndices
                    outPendingFaces

            else
                case Array.get p vertexData.positions of
                    Just position ->
                        case Array.get n vertexData.normals of
                            Just normal ->
                                addFaces frame
                                    vertexData
                                    smoothingGroup
                                    lineno
                                    remainingVertices
                                    elements
                                    (maxIndex + 1)
                                    (Array.set p (n :: maxIndex + 1 :: lookupArray) indexMap)
                                    ({ position = Point3d.placeIn frame position
                                     , normal = Direction3d.toVector (Direction3d.placeIn frame normal)
                                     }
                                        :: outFaceVertices
                                    )
                                    (maxIndex + 1 :: outIndices)
                                    outFaceIndices
                                    outPendingFaces

                            Nothing ->
                                formatError lineno "Index out of range"

                    Nothing ->
                        formatError lineno "Index out of range"

        [] ->
            let
                newFaceIndices =
                    case outIndices of
                        p1 :: remainingIndices ->
                            -- parser guarantees at least 3 face indices
                            groupIndices p1 remainingIndices outFaceIndices

                        [] ->
                            outFaceIndices
            in
            case elements of
                (FaceElement newLineno hasNormals newElementVertices) :: rest ->
                    if hasNormals then
                        addFaces frame vertexData smoothingGroup newLineno newElementVertices rest maxIndex indexMap outFaceVertices [] newFaceIndices outPendingFaces

                    else
                        addFaces frame
                            vertexData
                            smoothingGroup
                            newLineno
                            []
                            rest
                            maxIndex
                            indexMap
                            outFaceVertices
                            []
                            newFaceIndices
                            ({ lineno = newLineno, smoothingGroup = smoothingGroup, elementVertices = newElementVertices } :: outPendingFaces)

                [] ->
                    Ok { maxIndex = maxIndex, indexMap = indexMap, faceVertices = outFaceVertices, faceIndices = newFaceIndices, pendingFaces = outPendingFaces }


{-| Face- and vertex-level inner loop for the unified textured-faces pass.
Parallel to `addBumpyFaces`; see that function's documentation for the
fast-path and pending-path behaviour.
-}
addTexturedFaces : Frame3d Meters coordinates { defines : ObjCoordinates } -> VertexData -> AddIndexedFaces (TexturedFace coordinates)
addTexturedFaces frame vertexData smoothingGroup lineno elementVertices elements maxIndex indexMap outFaceVertices outIndices outFaceIndices outPendingFaces =
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
                    smoothingGroup
                    lineno
                    remainingVertices
                    elements
                    maxIndex
                    indexMap
                    outFaceVertices
                    (idx :: outIndices)
                    outFaceIndices
                    outPendingFaces

            else
                case Array.get p vertexData.positions of
                    Just position ->
                        case Array.get n vertexData.normals of
                            Just normal ->
                                case Array.get uv vertexData.uvs of
                                    Just uvCoord ->
                                        addTexturedFaces frame
                                            vertexData
                                            smoothingGroup
                                            lineno
                                            remainingVertices
                                            elements
                                            (maxIndex + 1)
                                            (Array.set p (uv :: n :: maxIndex + 1 :: lookupArray) indexMap)
                                            ({ position = Point3d.placeIn frame position
                                             , normal = Direction3d.toVector (Direction3d.placeIn frame normal)
                                             , uv = uvCoord
                                             }
                                                :: outFaceVertices
                                            )
                                            (maxIndex + 1 :: outIndices)
                                            outFaceIndices
                                            outPendingFaces

                                    Nothing ->
                                        formatError lineno "Index out of range"

                            Nothing ->
                                formatError lineno "Index out of range"

                    Nothing ->
                        formatError lineno "Index out of range"

        [] ->
            let
                newFaceIndices =
                    case outIndices of
                        p1 :: remainingIndices ->
                            -- parser guarantees at least 3 face indices
                            groupIndices p1 remainingIndices outFaceIndices

                        [] ->
                            outFaceIndices
            in
            case elements of
                (FaceElement newLineno hasNormals newElementVertices) :: remainingElements ->
                    if hasNormals then
                        addTexturedFaces frame vertexData smoothingGroup newLineno newElementVertices remainingElements maxIndex indexMap outFaceVertices [] newFaceIndices outPendingFaces

                    else
                        addTexturedFaces frame
                            vertexData
                            smoothingGroup
                            newLineno
                            []
                            remainingElements
                            maxIndex
                            indexMap
                            outFaceVertices
                            []
                            newFaceIndices
                            ({ lineno = newLineno, smoothingGroup = smoothingGroup, elementVertices = newElementVertices } :: outPendingFaces)

                [] ->
                    Ok { maxIndex = maxIndex, indexMap = indexMap, faceVertices = outFaceVertices, faceIndices = newFaceIndices, pendingFaces = outPendingFaces }


{-| outFaceVertices is accumulated in reverse order.
-}
generateFacesNormals :
    Frame3d Meters coordinates { defines : ObjCoordinates }
    -> VertexData
    -> List PendingFace
    -> Dict Int (List Int)
    -> List (Face coordinates)
    -> Int
    -> List ( Int, Int, Int )
    -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
    -> Bool
    -> ( List (Face coordinates), List ( Int, Int, Int ) )
generateFacesNormals frame vertexData pendingFaces vMap outFaceVertices outIdx outFaceIndices weightedNormals bitflags =
    case pendingFaces of
        [] ->
            ( outFaceVertices, outFaceIndices )

        pf :: remainingPendingFaces ->
            if pf.smoothingGroup == 0 then
                -- Flat shading: unshared vertices per face
                case pf.elementVertices of
                    v0 :: v1 :: remainingElementVertices ->
                        case Array.get v0.p vertexData.positions of
                            Just pos0 ->
                                case Array.get v1.p vertexData.positions of
                                    Just pos1 ->
                                        let
                                            pos0InFrame =
                                                Point3d.placeIn frame pos0

                                            pos1InFrame =
                                                Point3d.placeIn frame pos1

                                            ( newOutFaceVertices, newOutIdx, newOutFaceIndices ) =
                                                flatFacesNormals remainingElementVertices frame vertexData pos0InFrame pos1InFrame outFaceVertices outIdx outFaceIndices
                                        in
                                        generateFacesNormals frame vertexData remainingPendingFaces vMap newOutFaceVertices newOutIdx newOutFaceIndices weightedNormals bitflags

                                    Nothing ->
                                        generateFacesNormals frame vertexData remainingPendingFaces vMap outFaceVertices outIdx outFaceIndices weightedNormals bitflags

                            Nothing ->
                                generateFacesNormals frame vertexData remainingPendingFaces vMap outFaceVertices outIdx outFaceIndices weightedNormals bitflags

                    _ ->
                        generateFacesNormals frame vertexData remainingPendingFaces vMap outFaceVertices outIdx outFaceIndices weightedNormals bitflags

            else
                let
                    r =
                        buildFaceVertices weightedNormals bitflags pf.smoothingGroup pf.elementVertices frame vertexData vMap outFaceVertices outIdx [] outFaceIndices
                in
                generateFacesNormals frame vertexData remainingPendingFaces r.vMap r.faceVertices r.idx r.faceIndices weightedNormals bitflags


buildFaceVertices :
    Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
    -> Bool
    -> Int
    -> List Vertex
    -> Frame3d Meters coordinates { defines : ObjCoordinates }
    -> VertexData
    -> Dict Int (List Int)
    -> List (Face coordinates)
    -> Int
    -> List Int
    -> List ( Int, Int, Int )
    -> FaceVerticesResult coordinates
buildFaceVertices weightedNormals bitflags smoothingGroup elementVertices frame vertexData vMap outFaceVertices outIdx outIndices outFaceIndices =
    case elementVertices of
        [] ->
            case outIndices of
                i1 :: restIndices ->
                    { faceVertices = outFaceVertices, idx = outIdx, faceIndices = groupIndices i1 restIndices outFaceIndices, vMap = vMap }

                [] ->
                    { faceVertices = outFaceVertices, idx = outIdx, faceIndices = outFaceIndices, vMap = vMap }

        { p } :: remainingElementVertices ->
            let
                existingIdx =
                    case Dict.get p vMap of
                        Just entries ->
                            lookup1 smoothingGroup entries

                        Nothing ->
                            -1
            in
            if existingIdx > -1 then
                buildFaceVertices weightedNormals bitflags smoothingGroup remainingElementVertices frame vertexData vMap outFaceVertices outIdx (existingIdx :: outIndices) outFaceIndices

            else
                case lookupNormal bitflags p smoothingGroup weightedNormals of
                    Just normal ->
                        case Array.get p vertexData.positions of
                            Just position ->
                                buildFaceVertices
                                    weightedNormals
                                    bitflags
                                    smoothingGroup
                                    remainingElementVertices
                                    frame
                                    vertexData
                                    (Dict.insert p
                                        -- (p, smoothingGroup) is guaranteed unique here (lookup1 returned -1 above),
                                        -- so we always prepend a fresh pair; never overwrite an existing entry.
                                        (case Dict.get p vMap of
                                            Just outEntries ->
                                                smoothingGroup :: outIdx :: outEntries

                                            Nothing ->
                                                [ smoothingGroup, outIdx ]
                                        )
                                        vMap
                                    )
                                    ({ position = Point3d.placeIn frame position

                                     -- Normalize the area-weighted sum to get a unit normal.
                                     , normal = Vector3d.placeIn frame (Vector3d.normalize normal)
                                     }
                                        :: outFaceVertices
                                    )
                                    (outIdx + 1)
                                    (outIdx :: outIndices)
                                    outFaceIndices

                            Nothing ->
                                buildFaceVertices weightedNormals bitflags smoothingGroup remainingElementVertices frame vertexData vMap outFaceVertices outIdx outIndices outFaceIndices

                    Nothing ->
                        buildFaceVertices weightedNormals bitflags smoothingGroup remainingElementVertices frame vertexData vMap outFaceVertices outIdx outIndices outFaceIndices


{-| outFaceVertices is accumulated in reverse order.
-}
flatFacesNormals :
    List Vertex
    -> Frame3d Meters coordinates { defines : ObjCoordinates }
    -> VertexData
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> List (Face coordinates)
    -> Int
    -> List ( Int, Int, Int )
    -> ( List (Face coordinates), Int, List ( Int, Int, Int ) )
flatFacesNormals elementVertices frame vertexData pos0InFrame prevPosInFrame outFaceVertices outIdx outFaceIndices =
    case elementVertices of
        [] ->
            ( outFaceVertices, outIdx, outFaceIndices )

        vN :: remainingElementVertices ->
            case Array.get vN.p vertexData.positions of
                Just posN ->
                    let
                        posNInFrame =
                            Point3d.placeIn frame posN

                        triangleNormal =
                            Vector3d.normalize
                                (Vector3d.cross
                                    (Vector3d.from pos0InFrame prevPosInFrame)
                                    (Vector3d.from pos0InFrame posNInFrame)
                                )
                    in
                    flatFacesNormals remainingElementVertices
                        frame
                        vertexData
                        pos0InFrame
                        posNInFrame
                        ({ position = pos0InFrame, normal = triangleNormal }
                            :: { position = prevPosInFrame, normal = triangleNormal }
                            :: { position = posNInFrame, normal = triangleNormal }
                            :: outFaceVertices
                        )
                        (outIdx + 3)
                        (( outIdx, outIdx + 1, outIdx + 2 ) :: outFaceIndices)

                Nothing ->
                    flatFacesNormals remainingElementVertices frame vertexData pos0InFrame prevPosInFrame outFaceVertices outIdx outFaceIndices


{-| outFaceVertices is accumulated in reverse order.
-}
generateTexturedFacesNormals :
    Frame3d Meters coordinates { defines : ObjCoordinates }
    -> VertexData
    -> List PendingFace
    -> Dict Int (List Int)
    -> List (TexturedFace coordinates)
    -> Int
    -> List ( Int, Int, Int )
    -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
    -> Bool
    -> ( List (TexturedFace coordinates), List ( Int, Int, Int ) )
generateTexturedFacesNormals frame vertexData pendingFaces vMap outFaceVertices outIdx outFaceIndices weightedNormals bitflags =
    case pendingFaces of
        [] ->
            ( outFaceVertices, outFaceIndices )

        pf :: remainingPendingFaces ->
            if pf.smoothingGroup == 0 then
                -- Flat shading: unshared vertices per face
                case pf.elementVertices of
                    v0 :: (v1 :: remainingElementVertices) ->
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

                                                            faceNormal =
                                                                Vector3d.normalize
                                                                    (Vector3d.placeIn frame
                                                                        (polygonFanNormal vertexData (Point3d.toMeters pos0) (Point3d.toMeters pos1) remainingElementVertices Vector3d.zero)
                                                                    )

                                                            ( newOutFaceVertices, newOutIdx, newOutFaceIndices ) =
                                                                flatTexturedFacesNormals remainingElementVertices frame vertexData pos0InFrame pos1InFrame uv0 uv1 faceNormal outFaceVertices outIdx outFaceIndices
                                                        in
                                                        generateTexturedFacesNormals frame vertexData remainingPendingFaces vMap newOutFaceVertices newOutIdx newOutFaceIndices weightedNormals bitflags

                                                    Nothing ->
                                                        generateTexturedFacesNormals frame vertexData remainingPendingFaces vMap outFaceVertices outIdx outFaceIndices weightedNormals bitflags

                                            Nothing ->
                                                generateTexturedFacesNormals frame vertexData remainingPendingFaces vMap outFaceVertices outIdx outFaceIndices weightedNormals bitflags

                                    Nothing ->
                                        generateTexturedFacesNormals frame vertexData remainingPendingFaces vMap outFaceVertices outIdx outFaceIndices weightedNormals bitflags

                            Nothing ->
                                generateTexturedFacesNormals frame vertexData remainingPendingFaces vMap outFaceVertices outIdx outFaceIndices weightedNormals bitflags

                    _ ->
                        generateTexturedFacesNormals frame vertexData remainingPendingFaces vMap outFaceVertices outIdx outFaceIndices weightedNormals bitflags

            else
                buildTexturedFaceVertices frame vertexData remainingPendingFaces weightedNormals bitflags pf.smoothingGroup pf.elementVertices vMap outFaceVertices outIdx [] outFaceIndices


buildTexturedFaceVertices :
    Frame3d Meters coordinates { defines : ObjCoordinates }
    -> VertexData
    -> List PendingFace
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
buildTexturedFaceVertices frame vertexData pendingFaces weightedNormals bitflags smoothingGroup elementVertices vMap outFaceVertices outIdx outIndices outFaceIndices =
    case elementVertices of
        [] ->
            case outIndices of
                i1 :: remainingIndices ->
                    generateTexturedFacesNormals frame vertexData pendingFaces vMap outFaceVertices outIdx (groupIndices i1 remainingIndices outFaceIndices) weightedNormals bitflags

                [] ->
                    generateTexturedFacesNormals frame vertexData pendingFaces vMap outFaceVertices outIdx outFaceIndices weightedNormals bitflags

        { p, uv } :: remainingElementVertices ->
            let
                existingIdx =
                    case Dict.get p vMap of
                        Just entries ->
                            lookup2 smoothingGroup uv entries

                        Nothing ->
                            -1
            in
            if existingIdx > -1 then
                buildTexturedFaceVertices frame vertexData pendingFaces weightedNormals bitflags smoothingGroup remainingElementVertices vMap outFaceVertices outIdx (existingIdx :: outIndices) outFaceIndices

            else
                case lookupNormal bitflags p smoothingGroup weightedNormals of
                    Just normal ->
                        case Array.get uv vertexData.uvs of
                            Just uvCoord ->
                                case Array.get p vertexData.positions of
                                    Just position ->
                                        buildTexturedFaceVertices frame
                                            vertexData
                                            pendingFaces
                                            weightedNormals
                                            bitflags
                                            smoothingGroup
                                            remainingElementVertices
                                            (Dict.insert p
                                                -- (smoothingGroup, uv) is guaranteed unique here (lookup2 returned -1 above),
                                                -- so we always prepend a fresh triple; never overwrite an existing entry.
                                                (case Dict.get p vMap of
                                                    Just entries ->
                                                        smoothingGroup :: uv :: outIdx :: entries

                                                    Nothing ->
                                                        [ smoothingGroup, uv, outIdx ]
                                                )
                                                vMap
                                            )
                                            ({ position = Point3d.placeIn frame position

                                             -- Normalize the area-weighted sum to get a unit normal.
                                             , normal = Vector3d.placeIn frame (Vector3d.normalize normal)
                                             , uv = uvCoord
                                             }
                                                :: outFaceVertices
                                            )
                                            (outIdx + 1)
                                            (outIdx :: outIndices)
                                            outFaceIndices

                                    Nothing ->
                                        buildTexturedFaceVertices frame vertexData pendingFaces weightedNormals bitflags smoothingGroup remainingElementVertices vMap outFaceVertices outIdx outIndices outFaceIndices

                            Nothing ->
                                buildTexturedFaceVertices frame vertexData pendingFaces weightedNormals bitflags smoothingGroup remainingElementVertices vMap outFaceVertices outIdx outIndices outFaceIndices

                    Nothing ->
                        buildTexturedFaceVertices frame vertexData pendingFaces weightedNormals bitflags smoothingGroup remainingElementVertices vMap outFaceVertices outIdx outIndices outFaceIndices


{-| outFaceVertices is accumulated in reverse order.
-}
flatTexturedFacesNormals :
    List Vertex
    -> Frame3d Meters coordinates { defines : ObjCoordinates }
    -> VertexData
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> ( Float, Float )
    -> ( Float, Float )
    -> Vector3d Unitless coordinates
    -> List (TexturedFace coordinates)
    -> Int
    -> List ( Int, Int, Int )
    -> ( List (TexturedFace coordinates), Int, List ( Int, Int, Int ) )
flatTexturedFacesNormals elementVertices frame vertexData pos0InFrame prevPosInFrame uv0 prevUV normal outFaceVertices outIdx outFaceIndices =
    case elementVertices of
        [] ->
            ( outFaceVertices, outIdx, outFaceIndices )

        vN :: remainingElementVertices ->
            case Array.get vN.p vertexData.positions of
                Just posN ->
                    case Array.get vN.uv vertexData.uvs of
                        Just uvN ->
                            let
                                posNInFrame =
                                    Point3d.placeIn frame posN
                            in
                            flatTexturedFacesNormals remainingElementVertices
                                frame
                                vertexData
                                pos0InFrame
                                posNInFrame
                                uv0
                                uvN
                                normal
                                ({ position = pos0InFrame, normal = normal, uv = uv0 }
                                    :: { position = prevPosInFrame, normal = normal, uv = prevUV }
                                    :: { position = posNInFrame, normal = normal, uv = uvN }
                                    :: outFaceVertices
                                )
                                (outIdx + 3)
                                (( outIdx, outIdx + 1, outIdx + 2 ) :: outFaceIndices)

                        Nothing ->
                            flatTexturedFacesNormals remainingElementVertices frame vertexData pos0InFrame prevPosInFrame uv0 prevUV normal outFaceVertices outIdx outFaceIndices

                Nothing ->
                    flatTexturedFacesNormals remainingElementVertices frame vertexData pos0InFrame prevPosInFrame uv0 prevUV normal outFaceVertices outIdx outFaceIndices


smoothingGroupsSet : List Group -> Set Int
smoothingGroupsSet groups =
    Set.fromList (smoothingGroupsHelp groups [])


smoothingGroupsMask : List Group -> Int
smoothingGroupsMask groups =
    List.foldl Bitwise.or 0 (smoothingGroupsHelp groups [])


smoothingGroupsHelp : List Group -> List Int -> List Int
smoothingGroupsHelp groups outSmoothingGroups =
    case groups of
        [] ->
            outSmoothingGroups

        (Group record _ _ _) :: rest ->
            if record.smoothingGroup == 0 then
                smoothingGroupsHelp rest outSmoothingGroups

            else
                smoothingGroupsHelp rest (record.smoothingGroup :: outSmoothingGroups)


{-| Accumulate area-weighted normals for the smoothing-group reconstruction pass.

Iterates over `vertexData.fullGroups`, skipping groups whose smoothing-group ID
is not in `smoothingIds`. For each qualifying face the area-weighted face normal
(cross product, not yet normalised) is added to every corner's bucket.

The `keyFn` argument maps a `Vertex` and its smoothing-group ID to the
dictionary key for that corner. Callers pass:

    - `\v smoothingGroup -> ( v.p, smoothingGroup )` for `faces` (no UV component in key)
    - `\v smoothingGroup -> ( v.p, smoothingGroup, v.uv )` for `texturedFaces` / `bumpyFaces`

-}
collectWeightedNormals : VertexData -> List Group -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates )) -> Set Int -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
collectWeightedNormals vertexData groups weightedNormals filteredSmoothingGroups =
    collectWeightedNormalsHelp (\smoothingGroup -> Set.member smoothingGroup filteredSmoothingGroups) vertexData groups weightedNormals


collectBitflagWeightedNormals : VertexData -> List Group -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates )) -> Int -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
collectBitflagWeightedNormals vertexData groups weightedNormals allBits =
    collectWeightedNormalsHelp (\smoothingGroup -> Bitwise.and smoothingGroup allBits /= 0) vertexData groups weightedNormals


collectWeightedNormalsHelp : (Int -> Bool) -> VertexData -> List Group -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates )) -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
collectWeightedNormalsHelp matches vertexData groups outWeightedNormals =
    case groups of
        [] ->
            outWeightedNormals

        (Group { smoothingGroup } faceElements _ _) :: remainingGroups ->
            if matches smoothingGroup then
                collectWeightedNormalsHelp matches
                    vertexData
                    remainingGroups
                    (collectWeightedNormalsFaces vertexData smoothingGroup faceElements outWeightedNormals)

            else
                collectWeightedNormalsHelp matches vertexData remainingGroups outWeightedNormals


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
    -> { x : Float, y : Float, z : Float }
    -> List Vertex
    -> Vector3d Unitless ObjCoordinates
    -> Vector3d Unitless ObjCoordinates
polygonFanNormal vertexData p0 prevPos elementVertices normal =
    case elementVertices of
        vB :: remainingElementVertices ->
            case Array.get vB.p vertexData.positions of
                Just posB ->
                    let
                        pB =
                            Point3d.toMeters posB

                        ax =
                            prevPos.x - p0.x

                        ay =
                            prevPos.y - p0.y

                        az =
                            prevPos.z - p0.z

                        bx =
                            pB.x - p0.x

                        by =
                            pB.y - p0.y

                        bz =
                            pB.z - p0.z
                    in
                    polygonFanNormal vertexData
                        p0
                        pB
                        remainingElementVertices
                        (Vector3d.plus normal
                            (Vector3d.unitless
                                (by * az - bz * ay)
                                (bz * ax - bx * az)
                                (bx * ay - by * ax)
                            )
                        )

                Nothing ->
                    polygonFanNormal vertexData p0 prevPos remainingElementVertices normal

        _ ->
            normal


collectWeightedNormalsFaces : VertexData -> Int -> List FaceElement -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates )) -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
collectWeightedNormalsFaces vertexData smoothingGroup faceElements outWeightedNormals =
    case faceElements of
        [] ->
            outWeightedNormals

        (FaceElement _ _ ((v0 :: v1 :: ((_ :: _) as remainingElementVertices)) as elementVertices)) :: remainingFaceElements ->
            case Array.get v0.p vertexData.positions of
                Just pos0 ->
                    case Array.get v1.p vertexData.positions of
                        Just pos1 ->
                            let
                                normal =
                                    polygonFanNormal vertexData (Point3d.toMeters pos0) (Point3d.toMeters pos1) remainingElementVertices Vector3d.zero
                            in
                            collectWeightedNormalsFaces vertexData
                                smoothingGroup
                                remainingFaceElements
                                (collectWeightedNormalsVertices smoothingGroup normal elementVertices outWeightedNormals)

                        Nothing ->
                            collectWeightedNormalsFaces vertexData smoothingGroup remainingFaceElements outWeightedNormals

                Nothing ->
                    collectWeightedNormalsFaces vertexData smoothingGroup remainingFaceElements outWeightedNormals

        (FaceElement _ _ _) :: remainingFaceElements ->
            collectWeightedNormalsFaces vertexData smoothingGroup remainingFaceElements outWeightedNormals


lookupNormal : Bool -> Int -> Int -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates )) -> Maybe (Vector3d Unitless ObjCoordinates)
lookupNormal bitflags p smoothingGroup weightedNormals =
    case Dict.get p weightedNormals of
        Nothing ->
            Nothing

        Just entries ->
            lookupNormalHelp bitflags smoothingGroup entries Nothing


lookupNormalHelp : Bool -> Int -> List ( Int, Vector3d Unitless ObjCoordinates ) -> Maybe (Vector3d Unitless ObjCoordinates) -> Maybe (Vector3d Unitless ObjCoordinates)
lookupNormalHelp bitflags smoothingGroup entries outNormal =
    case entries of
        [] ->
            outNormal

        ( s, v ) :: remainingEntries ->
            let
                matches =
                    if bitflags then
                        Bitwise.and smoothingGroup s /= 0

                    else
                        smoothingGroup == s
            in
            lookupNormalHelp bitflags
                smoothingGroup
                remainingEntries
                (if matches then
                    case outNormal of
                        Nothing ->
                            Just v

                        Just existing ->
                            Just (Vector3d.plus existing v)

                 else
                    outNormal
                )


{-| Add `normal` to the entry for `smoothingGroup` in `entries`, or prepend a new entry if none exists.
Order within the list is not significant; `lookupNormal` scans the whole list.
-}
addNormal : Int -> Vector3d Unitless ObjCoordinates -> List ( Int, Vector3d Unitless ObjCoordinates ) -> List ( Int, Vector3d Unitless ObjCoordinates ) -> List ( Int, Vector3d Unitless ObjCoordinates )
addNormal smoothingGroup normal entries outEntries =
    case entries of
        [] ->
            ( smoothingGroup, normal ) :: outEntries

        (( currentSmoothingGroup, currentNormal ) as currentEntry) :: remainingEntries ->
            if smoothingGroup - currentSmoothingGroup == 0 then
                -- The cross product magnitude is proportional to triangle area,
                -- so larger triangles contribute more weight to the smoothed normal.
                -- Fold the existing vector into normal and drop the old entry from its position.
                addNormal smoothingGroup (Vector3d.plus normal currentNormal) remainingEntries outEntries

            else
                addNormal smoothingGroup normal remainingEntries (currentEntry :: outEntries)


collectWeightedNormalsVertices : Int -> Vector3d Unitless ObjCoordinates -> List Vertex -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates )) -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
collectWeightedNormalsVertices smoothingGroup normal elementVertices outWeightedNormals =
    case elementVertices of
        [] ->
            outWeightedNormals

        v :: remainingElementVertices ->
            -- Key by position: v.p alone is sufficient when each position belongs to one group,
            -- but the same geometric position can appear in multiple smoothing groups and each
            -- group needs its own normal, so smoothingGroup is stored in the inner list.
            collectWeightedNormalsVertices smoothingGroup
                normal
                remainingElementVertices
                (Dict.insert v.p
                    (case Dict.get v.p outWeightedNormals of
                        Just entries ->
                            addNormal smoothingGroup normal entries []

                        Nothing ->
                            [ ( smoothingGroup, normal ) ]
                    )
                    outWeightedNormals
                )


{-| Adds tangents to the vertices, initializing them to zero. This is necessary for the tangent accumulation.
Note: this reverses the order of vertices.
-}
addZeroTangents :
    List (TexturedFace coordinates)
    -> List { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ), tangent : Vector3d Unitless coordinates, bitangent : Vector3d Unitless coordinates }
    -> List { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ), tangent : Vector3d Unitless coordinates, bitangent : Vector3d Unitless coordinates }
addZeroTangents faceVertices outFaceVertices =
    case faceVertices of
        [] ->
            outFaceVertices

        { position, normal, uv } :: remainingFaceVertices ->
            addZeroTangents remainingFaceVertices
                ({ position = position
                 , normal = normal
                 , uv = uv
                 , tangent = Vector3d.zero
                 , bitangent = Vector3d.zero
                 }
                    :: outFaceVertices
                )


computeTangents :
    List String
    -> List (TexturedFace coordinates)
    -> List ( Int, Int, Int )
    -> Result String (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ), tangent : Vector3d Unitless coordinates, tangentBasisIsRightHanded : Bool })
computeTangents filters faceVertices faceIndices =
    let
        faceVerticesArray =
            Array.fromList (addZeroTangents faceVertices [])

        orthogonalizedFaceVertices =
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
                (computeTangentsHelp faceIndices faceVerticesArray)
    in
    buildMeshResult filters orthogonalizedFaceVertices faceIndices


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
computeTangentsHelp faceIndices faceVertices =
    case faceIndices of
        [] ->
            faceVertices

        ( i1, i2, i3 ) :: remainingFaceIndices ->
            case Array.get i1 faceVertices of
                Just vertex1 ->
                    case Array.get i2 faceVertices of
                        Just vertex2 ->
                            case Array.get i3 faceVertices of
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
                                    computeTangentsHelp remainingFaceIndices
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
                                                    faceVertices
                                                )
                                            )
                                        )

                                Nothing ->
                                    computeTangentsHelp remainingFaceIndices faceVertices

                        Nothing ->
                            computeTangentsHelp remainingFaceIndices faceVertices

                Nothing ->
                    computeTangentsHelp remainingFaceIndices faceVertices
