module Obj.Internal.Faces exposing
    ( bumpyFaces
    , faces
    , texturedFaces
    )

import Array
import Direction3d
import Frame3d exposing (Frame3d)
import Length exposing (Meters)
import Obj.Internal.IndexMap as IndexMap exposing (IndexMap, Key2, Key3)
import Obj.Internal.MeshHelpers exposing (buildMeshResult, groupIndices)
import Obj.Internal.Parse
    exposing
        ( FaceElement(..)
        , Group(..)
        , Vertex
        , VertexData
        , formatError
        )
import Obj.Internal.SmoothNormals as SmoothNormals exposing (SmoothNormals)
import Obj.Internal.Tangents as Tangents
import Point3d exposing (Point3d)
import Quantity exposing (Unitless)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


{-| Pipeline for faces / texturedFaces:

Fast path (all faces have explicit normals in OBJ):

      triangularMesh / addFaces
        outVertices:    accumulated by prepending         → [vN,..,v1] (reversed)
        outFaceIndices: two reversals restore parse order → [f1,..,fN] (parse order)

      Array.fromList (List.reverse outVertices) → [v1,..,vN] ✓
      outFaceIndices passed as-is               → [f1,..,fN] ✓

Deferred path (some faces need normals reconstructed):

      triangularMesh / addFaces
        outVertices:        [vM,..,v1]   (reversed)
        outFaceIndices:     [f1,..,fM]   (parse order, fast-path faces only)
        smoothPendingFaces: [ps1,..,psK] (parse order, smooth deferred faces)
        flatPendingFaces:   [pf1,..,pfJ] (parse order, flat deferred faces)

      addSmoothFaces then addFlatFaces
        (each prepends new outVertices/outFaceIndices onto existing state)
        outVertices:    [vN,..,vM+1 | vM,..,v1]                       (still reversed)
        outFaceIndices: [flat reversed | smooth reversed | f1,..,fM]

      Array.fromList (List.reverse outVertices) → [v1,..,vN]                   ✓
      List.reverse outFaceIndices               → [fM,..,f1, smooth.., flat..] ✓

-}
faces :
    Frame3d Meters coordinates { defines : objCoordinates }
    -> Bool
    -> VertexData objCoordinates
    -> List String
    -> List Group
    -> Result String (TriangularMesh (Face coordinates))
faces frame bitflags vertexData filters filteredGroups =
    case triangularMesh (addFaces frame vertexData) filteredGroups -1 (IndexMap.init2 vertexData.emptyIndexMap) [] [] [] [] of
        Err error ->
            Err error

        Ok { flatPendingFaces, smoothPendingFaces, faceVertices, faceIndices, maxIndex } ->
            case ( flatPendingFaces, smoothPendingFaces ) of
                ( [], [] ) ->
                    buildMeshResult filters (Array.fromList (List.reverse faceVertices)) faceIndices

                _ ->
                    let
                        ( verticesAfterSmooth, idxAfterSmooth, indicesAfterSmooth ) =
                            case smoothPendingFaces of
                                [] ->
                                    ( faceVertices, maxIndex + 1, faceIndices )

                                ( smoothingGroup, FaceElement _ _ elementVertices ) :: remainingSmoothPendingFaces ->
                                    let
                                        smoothNormals =
                                            if bitflags then
                                                SmoothNormals.bitflag vertexData.positions filteredGroups

                                            else
                                                SmoothNormals.exact vertexData.positions filteredGroups
                                    in
                                    addSmoothFaces frame vertexData smoothNormals smoothingGroup elementVertices remainingSmoothPendingFaces (IndexMap.init2 vertexData.emptyIndexMap) faceVertices (maxIndex + 1) [] faceIndices

                        ( finalVertices, finalIndices ) =
                            addFlatFaces frame vertexData [] flatPendingFaces Point3d.origin Point3d.origin verticesAfterSmooth idxAfterSmooth indicesAfterSmooth
                    in
                    buildMeshResult filters (Array.fromList (List.reverse finalVertices)) (List.reverse finalIndices)


texturedFaces :
    Frame3d Meters coordinates { defines : objCoordinates }
    -> Bool
    -> VertexData objCoordinates
    -> List String
    -> List Group
    -> Result String (TriangularMesh (TexturedFace coordinates))
texturedFaces frame bitflags vertexData filters filteredGroups =
    case triangularMesh (addTexturedFaces frame vertexData) filteredGroups -1 (IndexMap.init3 vertexData.emptyIndexMap) [] [] [] [] of
        Err error ->
            Err error

        Ok { flatPendingFaces, smoothPendingFaces, faceVertices, faceIndices, maxIndex } ->
            case ( flatPendingFaces, smoothPendingFaces ) of
                ( [], [] ) ->
                    buildMeshResult filters (Array.fromList (List.reverse faceVertices)) faceIndices

                _ ->
                    let
                        ( verticesAfterSmooth, idxAfterSmooth, indicesAfterSmooth ) =
                            case smoothPendingFaces of
                                [] ->
                                    ( faceVertices, maxIndex + 1, faceIndices )

                                ( smoothingGroup, FaceElement _ _ elementVertices ) :: remainingSmoothPendingFaces ->
                                    let
                                        smoothNormals =
                                            if bitflags then
                                                SmoothNormals.bitflag vertexData.positions filteredGroups

                                            else
                                                SmoothNormals.exact vertexData.positions filteredGroups
                                    in
                                    addSmoothTexturedFaces frame vertexData smoothNormals smoothingGroup elementVertices remainingSmoothPendingFaces (IndexMap.init3 vertexData.emptyIndexMap) faceVertices (maxIndex + 1) [] faceIndices

                        ( finalVertices, finalIndices ) =
                            addFlatTexturedFaces frame vertexData [] flatPendingFaces Point3d.origin Point3d.origin ( 0, 0 ) ( 0, 0 ) verticesAfterSmooth idxAfterSmooth indicesAfterSmooth
                    in
                    buildMeshResult filters (Array.fromList (List.reverse finalVertices)) (List.reverse finalIndices)


{-| Pipeline for bumpyFaces:

Fast path (all faces have explicit normals in OBJ):

      triangularMesh / addTexturedFaces
        outVertices: accumulated by prepending         → [vN,..,v1] (reversed)
        outFaceIndices:  two reversals restore parse order → [f1,..,fN] (parse order)

      buildMeshResult / Tangents.compute outVertices outFaceIndices
        outVertices received reversed → addZeroTangents un-reverses → [v1,..,vN] ✓
        outFaceIndices passed as-is                                     → [f1,..,fN] ✓

Deferred path (some faces need normals reconstructed):

      triangularMesh / addTexturedFaces
        outVertices:    [vM,..,v1]   (reversed)
        outFaceIndices:     [f1,..,fM]   (parse order, fast-path faces only)
        smoothPendingFaces: [ps1,..,psK] (parse order)
        flatPendingFaces:   [pf1,..,pfJ] (parse order)

      addSmoothTexturedFaces then addFlatTexturedFaces
        outVertices: [vN,..,vM+1 | vM,..,v1]                      (still reversed)
        outFaceIndices:  [flat reversed | smooth reversed | f1,..,fM]

      buildMeshResult / Tangents.compute outVertices (List.reverse outFaceIndices)
        outVertices received reversed → addZeroTangents un-reverses → [v1,..,vN] ✓
        List.reverse outFaceIndices                                     → correct order ✓

-}
bumpyFaces :
    Frame3d Meters coordinates { defines : objCoordinates }
    -> Bool
    -> VertexData objCoordinates
    -> List String
    -> List Group
    -> Result String (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ), tangent : Vector3d Unitless coordinates, tangentBasisIsRightHanded : Bool })
bumpyFaces frame bitflags vertexData filters filteredGroups =
    case triangularMesh (addTexturedFaces frame vertexData) filteredGroups -1 (IndexMap.init3 vertexData.emptyIndexMap) [] [] [] [] of
        Err error ->
            Err error

        Ok { flatPendingFaces, smoothPendingFaces, faceVertices, maxIndex, faceIndices } ->
            case ( flatPendingFaces, smoothPendingFaces ) of
                ( [], [] ) ->
                    buildMeshResult filters (Tangents.compute faceVertices faceIndices) faceIndices

                _ ->
                    let
                        ( verticesAfterSmooth, idxAfterSmooth, indicesAfterSmooth ) =
                            case smoothPendingFaces of
                                [] ->
                                    ( faceVertices, maxIndex + 1, faceIndices )

                                ( newSmoothingGroup, FaceElement _ _ newElementVertices ) :: remainingSmoothPendingFaces ->
                                    let
                                        smoothNormals =
                                            if bitflags then
                                                SmoothNormals.bitflag vertexData.positions filteredGroups

                                            else
                                                SmoothNormals.exact vertexData.positions filteredGroups
                                    in
                                    addSmoothTexturedFaces frame vertexData smoothNormals newSmoothingGroup newElementVertices remainingSmoothPendingFaces (IndexMap.init3 vertexData.emptyIndexMap) faceVertices (maxIndex + 1) [] faceIndices

                        ( finalVertices, finalIndices ) =
                            addFlatTexturedFaces frame vertexData [] flatPendingFaces Point3d.origin Point3d.origin ( 0, 0 ) ( 0, 0 ) verticesAfterSmooth idxAfterSmooth indicesAfterSmooth

                        reversedFinalIndices =
                            List.reverse finalIndices
                    in
                    buildMeshResult filters (Tangents.compute finalVertices reversedFinalIndices) reversedFinalIndices


type alias Face coordinates =
    { position : Point3d Meters coordinates
    , normal : Vector3d Unitless coordinates
    }


type alias TexturedFace coordinates =
    { position : Point3d Meters coordinates
    , normal : Vector3d Unitless coordinates
    , uv : ( Float, Float )
    }


type alias IndexedFaces a k =
    { maxIndex : Int
    , indexMap : IndexMap k
    , faceVertices : List a
    , faceIndices : List ( Int, Int, Int )
    , flatPendingFaces : List FaceElement
    , smoothPendingFaces : List ( Int, FaceElement )
    }


{-| Like AddIndexedTriangles, but also carries a smoothing group and collects
faces that lack normals into flat/smooth pending lists rather than failing.
-}
type alias AddIndexedFaces a k =
    Int
    -> Int
    -> List Vertex
    -> List FaceElement
    -> Int
    -> IndexMap k
    -> List a
    -> List Int
    -> List ( Int, Int, Int )
    -> List FaceElement
    -> List ( Int, FaceElement )
    -> Result String (IndexedFaces a k)


{-| Shared group-level driver for `faces`, `texturedFaces`, and `bumpyFaces`
passes.

Iterates over every `Group` in turn, dispatching each group's face elements to
`add`. Faces whose vertices all have explicit normals are handled immediately by
the fast path; faces missing a normal are appended to `outFlatPendingFaces` (if
smoothingGroup == 0) or `outSmoothPendingFaces` (if smoothingGroup > 0) for the
reconstruction pass.

When both pending lists are empty on return the caller can build the final mesh
directly from `outFaceIndices` and `outVertices` without any reconstruction
work.

-}
triangularMesh :
    AddIndexedFaces a k
    -> List Group
    -> Int
    -> IndexMap k
    -> List a
    -> List ( Int, Int, Int )
    -> List FaceElement
    -> List ( Int, FaceElement )
    -> Result String (IndexedFaces a k)
triangularMesh add groups maxIndex indexMap outVertices outFaceIndices outFlatPendingFaces outSmoothPendingFaces =
    case groups of
        (Group record (((FaceElement lineno hasNormals elementVertices) as faceElement) :: faceElements) _ _) :: remainingGroups ->
            let
                ( firstElementVertices, newFlatPending, newSmoothPending ) =
                    if hasNormals then
                        ( elementVertices, outFlatPendingFaces, outSmoothPendingFaces )

                    else if record.smoothingGroup == 0 then
                        ( [], faceElement :: outFlatPendingFaces, outSmoothPendingFaces )

                    else
                        ( [], outFlatPendingFaces, ( record.smoothingGroup, faceElement ) :: outSmoothPendingFaces )
            in
            case add record.smoothingGroup lineno firstElementVertices faceElements maxIndex indexMap outVertices [] outFaceIndices newFlatPending newSmoothPending of
                Ok newState ->
                    triangularMesh add remainingGroups newState.maxIndex newState.indexMap newState.faceVertices newState.faceIndices newState.flatPendingFaces newState.smoothPendingFaces

                Err error ->
                    Err error

        (Group _ [] _ _) :: remainingGroups ->
            -- skip an empty group
            triangularMesh add remainingGroups maxIndex indexMap outVertices outFaceIndices outFlatPendingFaces outSmoothPendingFaces

        [] ->
            Ok { maxIndex = maxIndex, indexMap = indexMap, faceVertices = outVertices, faceIndices = outFaceIndices, flatPendingFaces = outFlatPendingFaces, smoothPendingFaces = outSmoothPendingFaces }


{-| Face- and vertex-level inner loop for the unified faces pass.

Note: the dedup key is `(n)` alone via `lookup1` (no UV component), and the
indexMap stores `(n, vertexIdx)` pairs rather than triples.

-}
addFaces : Frame3d Meters coordinates { defines : objCoordinates } -> VertexData objCoordinates -> AddIndexedFaces (Face coordinates) Key2
addFaces frame vertexData smoothingGroup lineno elementVertices elements maxIndex indexMap outVertices outIndices outFaceIndices outFlatPendingFaces outSmoothPendingFaces =
    case elementVertices of
        { p, n } :: remainingVertices ->
            let
                idx =
                    IndexMap.get2 p n indexMap
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
                    outVertices
                    (idx :: outIndices)
                    outFaceIndices
                    outFlatPendingFaces
                    outSmoothPendingFaces

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
                                    (IndexMap.insert2 p n (maxIndex + 1) indexMap)
                                    ({ position = Point3d.placeIn frame position
                                     , normal = Direction3d.toVector (Direction3d.placeIn frame normal)
                                     }
                                        :: outVertices
                                    )
                                    (maxIndex + 1 :: outIndices)
                                    outFaceIndices
                                    outFlatPendingFaces
                                    outSmoothPendingFaces

                            Nothing ->
                                formatError lineno "Index out of range"

                    Nothing ->
                        formatError lineno "Index out of range"

        [] ->
            let
                newFaceIndices =
                    case outIndices of
                        p1 :: remainingIndices ->
                            groupIndices p1 remainingIndices outFaceIndices

                        [] ->
                            outFaceIndices
            in
            case elements of
                ((FaceElement newLineno hasNormals newElementVertices) as newFaceElement) :: remainingFaceElements ->
                    if hasNormals then
                        addFaces frame vertexData smoothingGroup newLineno newElementVertices remainingFaceElements maxIndex indexMap outVertices [] newFaceIndices outFlatPendingFaces outSmoothPendingFaces

                    else if smoothingGroup == 0 then
                        addFaces frame
                            vertexData
                            smoothingGroup
                            newLineno
                            []
                            remainingFaceElements
                            maxIndex
                            indexMap
                            outVertices
                            []
                            newFaceIndices
                            (newFaceElement :: outFlatPendingFaces)
                            outSmoothPendingFaces

                    else
                        addFaces frame
                            vertexData
                            smoothingGroup
                            newLineno
                            []
                            remainingFaceElements
                            maxIndex
                            indexMap
                            outVertices
                            []
                            newFaceIndices
                            outFlatPendingFaces
                            (( smoothingGroup, newFaceElement ) :: outSmoothPendingFaces)

                [] ->
                    Ok { maxIndex = maxIndex, indexMap = indexMap, faceVertices = outVertices, faceIndices = newFaceIndices, flatPendingFaces = outFlatPendingFaces, smoothPendingFaces = outSmoothPendingFaces }


addTexturedFaces : Frame3d Meters coordinates { defines : objCoordinates } -> VertexData objCoordinates -> AddIndexedFaces (TexturedFace coordinates) Key3
addTexturedFaces frame vertexData smoothingGroup lineno elementVertices elements maxIndex indexMap outVertices outIndices outFaceIndices outFlatPendingFaces outSmoothPendingFaces =
    case elementVertices of
        { p, uv, n } :: remainingVertices ->
            let
                idx =
                    IndexMap.get3 p uv n indexMap
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
                    outVertices
                    (idx :: outIndices)
                    outFaceIndices
                    outFlatPendingFaces
                    outSmoothPendingFaces

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
                                            (IndexMap.insert3 p uv n (maxIndex + 1) indexMap)
                                            ({ position = Point3d.placeIn frame position
                                             , normal = Direction3d.toVector (Direction3d.placeIn frame normal)
                                             , uv = uvCoord
                                             }
                                                :: outVertices
                                            )
                                            (maxIndex + 1 :: outIndices)
                                            outFaceIndices
                                            outFlatPendingFaces
                                            outSmoothPendingFaces

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
                            groupIndices p1 remainingIndices outFaceIndices

                        [] ->
                            outFaceIndices
            in
            case elements of
                ((FaceElement newLineno hasNormals newElementVertices) as newFaceElement) :: remainingElements ->
                    if hasNormals then
                        addTexturedFaces frame vertexData smoothingGroup newLineno newElementVertices remainingElements maxIndex indexMap outVertices [] newFaceIndices outFlatPendingFaces outSmoothPendingFaces

                    else if smoothingGroup == 0 then
                        addTexturedFaces frame
                            vertexData
                            smoothingGroup
                            newLineno
                            []
                            remainingElements
                            maxIndex
                            indexMap
                            outVertices
                            []
                            newFaceIndices
                            (newFaceElement :: outFlatPendingFaces)
                            outSmoothPendingFaces

                    else
                        addTexturedFaces frame
                            vertexData
                            smoothingGroup
                            newLineno
                            []
                            remainingElements
                            maxIndex
                            indexMap
                            outVertices
                            []
                            newFaceIndices
                            outFlatPendingFaces
                            (( smoothingGroup, newFaceElement ) :: outSmoothPendingFaces)

                [] ->
                    Ok { maxIndex = maxIndex, indexMap = indexMap, faceVertices = outVertices, faceIndices = newFaceIndices, flatPendingFaces = outFlatPendingFaces, smoothPendingFaces = outSmoothPendingFaces }


{-| Smooth normal generation: combined vertex- and face-level loop for non-textured faces.

Analogous to `addFaces` but for the second pass: processes smooth pending faces
vertex by vertex, looking up area-weighted normals from `smoothNormals`.
Deduplicates via `smoothIndexMap` (keyed by position index, storing smoothingGroup/vertexIdx pairs)
so vertices shared across smooth faces within the same smoothing group are emitted once.

When `elementVertices` is exhausted the next pending face is started inline.

-}
addSmoothFaces :
    Frame3d Meters coordinates { defines : objCoordinates }
    -> VertexData objCoordinates
    -> SmoothNormals objCoordinates
    -> Int
    -> List Vertex
    -> List ( Int, FaceElement )
    -> IndexMap Key2
    -> List (Face coordinates)
    -> Int
    -> List Int
    -> List ( Int, Int, Int )
    -> ( List (Face coordinates), Int, List ( Int, Int, Int ) )
addSmoothFaces frame vertexData smoothNormals smoothingGroup elementVertices pendingFaces smoothIndexMap outVertices outIdx outIndices outFaceIndices =
    case elementVertices of
        [] ->
            let
                newFaceIndices =
                    case outIndices of
                        i1 :: remainingIndices ->
                            groupIndices i1 remainingIndices outFaceIndices

                        [] ->
                            outFaceIndices
            in
            case pendingFaces of
                ( newSmoothingGroup, FaceElement _ _ newElementVertices ) :: remainingPendingFaces ->
                    addSmoothFaces frame vertexData smoothNormals newSmoothingGroup newElementVertices remainingPendingFaces smoothIndexMap outVertices outIdx [] newFaceIndices

                [] ->
                    ( outVertices, outIdx, newFaceIndices )

        { p } :: remainingElementVertices ->
            let
                existingIdx =
                    IndexMap.get2 p smoothingGroup smoothIndexMap
            in
            if existingIdx > -1 then
                addSmoothFaces frame vertexData smoothNormals smoothingGroup remainingElementVertices pendingFaces smoothIndexMap outVertices outIdx (existingIdx :: outIndices) outFaceIndices

            else
                case SmoothNormals.get p smoothingGroup smoothNormals of
                    Just normal ->
                        case Array.get p vertexData.positions of
                            Just position ->
                                addSmoothFaces frame
                                    vertexData
                                    smoothNormals
                                    smoothingGroup
                                    remainingElementVertices
                                    pendingFaces
                                    -- (p, smoothingGroup) is guaranteed unique here (get1 returned -1 above),
                                    -- so we always prepend a fresh pair; never overwrite an existing entry.
                                    (IndexMap.insert2 p smoothingGroup outIdx smoothIndexMap)
                                    ({ position = Point3d.placeIn frame position
                                     , normal = Vector3d.placeIn frame normal
                                     }
                                        :: outVertices
                                    )
                                    (outIdx + 1)
                                    (outIdx :: outIndices)
                                    outFaceIndices

                            Nothing ->
                                addSmoothFaces frame vertexData smoothNormals smoothingGroup remainingElementVertices pendingFaces smoothIndexMap outVertices outIdx outIndices outFaceIndices

                    Nothing ->
                        addSmoothFaces frame vertexData smoothNormals smoothingGroup remainingElementVertices pendingFaces smoothIndexMap outVertices outIdx outIndices outFaceIndices


{-| Smooth normal generation: combined vertex- and face-level loop for textured faces.

Analogous to `addFacesWithNormals` but for `TexturedFace`. Deduplicates on
`(smoothingGroup, uv)` via `lookup2` / triples in `smoothIndexMap`.

-}
addSmoothTexturedFaces :
    Frame3d Meters coordinates { defines : objCoordinates }
    -> VertexData objCoordinates
    -> SmoothNormals objCoordinates
    -> Int
    -> List Vertex
    -> List ( Int, FaceElement )
    -> IndexMap Key3
    -> List (TexturedFace coordinates)
    -> Int
    -> List Int
    -> List ( Int, Int, Int )
    -> ( List (TexturedFace coordinates), Int, List ( Int, Int, Int ) )
addSmoothTexturedFaces frame vertexData smoothNormals smoothingGroup elementVertices pendingFaces smoothIndexMap outVertices outIdx outIndices outFaceIndices =
    case elementVertices of
        [] ->
            let
                newFaceIndices =
                    case outIndices of
                        i1 :: remainingIndices ->
                            groupIndices i1 remainingIndices outFaceIndices

                        [] ->
                            outFaceIndices
            in
            case pendingFaces of
                ( newSmoothingGroup, FaceElement _ _ newElementVertices ) :: remainingPendingFaces ->
                    addSmoothTexturedFaces frame vertexData smoothNormals newSmoothingGroup newElementVertices remainingPendingFaces smoothIndexMap outVertices outIdx [] newFaceIndices

                [] ->
                    ( outVertices, outIdx, newFaceIndices )

        { p, uv } :: remainingElementVertices ->
            let
                existingIdx =
                    IndexMap.get3 p smoothingGroup uv smoothIndexMap
            in
            if existingIdx > -1 then
                addSmoothTexturedFaces frame vertexData smoothNormals smoothingGroup remainingElementVertices pendingFaces smoothIndexMap outVertices outIdx (existingIdx :: outIndices) outFaceIndices

            else
                case SmoothNormals.get p smoothingGroup smoothNormals of
                    Just normal ->
                        case Array.get uv vertexData.uvs of
                            Just uvCoord ->
                                case Array.get p vertexData.positions of
                                    Just position ->
                                        addSmoothTexturedFaces frame
                                            vertexData
                                            smoothNormals
                                            smoothingGroup
                                            remainingElementVertices
                                            pendingFaces
                                            -- (smoothingGroup, uv) is guaranteed unique here (get2 returned -1 above),
                                            -- so we always prepend a fresh triple; never overwrite an existing entry.
                                            (IndexMap.insert3 p smoothingGroup uv outIdx smoothIndexMap)
                                            ({ position = Point3d.placeIn frame position
                                             , normal = Vector3d.placeIn frame normal
                                             , uv = uvCoord
                                             }
                                                :: outVertices
                                            )
                                            (outIdx + 1)
                                            (outIdx :: outIndices)
                                            outFaceIndices

                                    Nothing ->
                                        addSmoothTexturedFaces frame vertexData smoothNormals smoothingGroup remainingElementVertices pendingFaces smoothIndexMap outVertices outIdx outIndices outFaceIndices

                            Nothing ->
                                addSmoothTexturedFaces frame vertexData smoothNormals smoothingGroup remainingElementVertices pendingFaces smoothIndexMap outVertices outIdx outIndices outFaceIndices

                    Nothing ->
                        addSmoothTexturedFaces frame vertexData smoothNormals smoothingGroup remainingElementVertices pendingFaces smoothIndexMap outVertices outIdx outIndices outFaceIndices


{-| Flat normal generation: combined vertex- and face-level loop for non-textured faces.

Processes flat pending faces (smoothingGroup == 0) vertex by vertex using a
triangle fan. Each triangle gets three unshared vertices. The normal is computed
per-triangle as the cross product of the two edges from pos0.

`pos0` is fixed for the lifetime of the current face; `prevPos` rotates with each
step. When `elementVertices` is exhausted the next flat face is bootstrapped inline:
its first two positions become the new `pos0` / `prevPos` and processing continues
from vertex 2 onward.

Call with `[] flatPendingFaces Point3d.origin Point3d.origin` to start.

-}
addFlatFaces :
    Frame3d Meters coordinates { defines : objCoordinates }
    -> VertexData objCoordinates
    -> List Vertex
    -> List FaceElement
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> List (Face coordinates)
    -> Int
    -> List ( Int, Int, Int )
    -> ( List (Face coordinates), List ( Int, Int, Int ) )
addFlatFaces frame vertexData elementVertices pendingFaces pos0 prevPos outVertices outIdx outFaceIndices =
    case elementVertices of
        { p } :: remainingElementVertices ->
            case Array.get p vertexData.positions of
                Just posN ->
                    let
                        posNInFrame =
                            Point3d.placeIn frame posN

                        triangleNormal =
                            Vector3d.normalize
                                (Vector3d.cross
                                    (Vector3d.from pos0 prevPos)
                                    (Vector3d.from pos0 posNInFrame)
                                )
                    in
                    addFlatFaces frame
                        vertexData
                        remainingElementVertices
                        pendingFaces
                        pos0
                        posNInFrame
                        ({ position = pos0, normal = triangleNormal }
                            :: { position = prevPos, normal = triangleNormal }
                            :: { position = posNInFrame, normal = triangleNormal }
                            :: outVertices
                        )
                        (outIdx + 3)
                        (( outIdx, outIdx + 1, outIdx + 2 ) :: outFaceIndices)

                Nothing ->
                    addFlatFaces frame vertexData remainingElementVertices pendingFaces pos0 prevPos outVertices outIdx outFaceIndices

        [] ->
            case pendingFaces of
                (FaceElement _ _ nextVertices) :: remainingFaces ->
                    case nextVertices of
                        v0 :: v1 :: remainingElementVertices ->
                            case Array.get v0.p vertexData.positions of
                                Just pos0New ->
                                    case Array.get v1.p vertexData.positions of
                                        Just pos1New ->
                                            addFlatFaces frame vertexData remainingElementVertices remainingFaces (Point3d.placeIn frame pos0New) (Point3d.placeIn frame pos1New) outVertices outIdx outFaceIndices

                                        Nothing ->
                                            addFlatFaces frame vertexData [] remainingFaces pos0 prevPos outVertices outIdx outFaceIndices

                                Nothing ->
                                    addFlatFaces frame vertexData [] remainingFaces pos0 prevPos outVertices outIdx outFaceIndices

                        _ ->
                            addFlatFaces frame vertexData [] remainingFaces pos0 prevPos outVertices outIdx outFaceIndices

                [] ->
                    ( outVertices, outFaceIndices )


{-| Flat normal generation: combined vertex- and face-level loop for textured faces.

Like `addFlatFaces` but for `TexturedFace`. The normal is computed per-triangle
as the cross product of the two edges from pos0.

Call with `[] flatPendingFaces Point3d.origin Point3d.origin (0,0) (0,0)`
to start.

-}
addFlatTexturedFaces :
    Frame3d Meters coordinates { defines : objCoordinates }
    -> VertexData objCoordinates
    -> List Vertex
    -> List FaceElement
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> ( Float, Float )
    -> ( Float, Float )
    -> List (TexturedFace coordinates)
    -> Int
    -> List ( Int, Int, Int )
    -> ( List (TexturedFace coordinates), List ( Int, Int, Int ) )
addFlatTexturedFaces frame vertexData elementVertices pendingFaces pos0 prevPos uv0 prevUV outVertices outIdx outFaceIndices =
    case elementVertices of
        vN :: remainingElementVertices ->
            case Array.get vN.p vertexData.positions of
                Just posN ->
                    case Array.get vN.uv vertexData.uvs of
                        Just uvN ->
                            let
                                posNInFrame =
                                    Point3d.placeIn frame posN

                                triangleNormal =
                                    Vector3d.normalize
                                        (Vector3d.cross
                                            (Vector3d.from pos0 prevPos)
                                            (Vector3d.from pos0 posNInFrame)
                                        )
                            in
                            addFlatTexturedFaces frame
                                vertexData
                                remainingElementVertices
                                pendingFaces
                                pos0
                                posNInFrame
                                uv0
                                uvN
                                ({ position = pos0, normal = triangleNormal, uv = uv0 }
                                    :: { position = prevPos, normal = triangleNormal, uv = prevUV }
                                    :: { position = posNInFrame, normal = triangleNormal, uv = uvN }
                                    :: outVertices
                                )
                                (outIdx + 3)
                                (( outIdx, outIdx + 1, outIdx + 2 ) :: outFaceIndices)

                        Nothing ->
                            addFlatTexturedFaces frame vertexData remainingElementVertices pendingFaces pos0 prevPos uv0 prevUV outVertices outIdx outFaceIndices

                Nothing ->
                    addFlatTexturedFaces frame vertexData remainingElementVertices pendingFaces pos0 prevPos uv0 prevUV outVertices outIdx outFaceIndices

        [] ->
            case pendingFaces of
                (FaceElement _ _ nextVertices) :: remainingFaces ->
                    case nextVertices of
                        v0 :: v1 :: remainingElementVertices ->
                            case Array.get v0.p vertexData.positions of
                                Just pos0New ->
                                    case Array.get v1.p vertexData.positions of
                                        Just pos1New ->
                                            case Array.get v0.uv vertexData.uvs of
                                                Just uv0New ->
                                                    case Array.get v1.uv vertexData.uvs of
                                                        Just uv1New ->
                                                            let
                                                                pos0InFrame =
                                                                    Point3d.placeIn frame pos0New

                                                                pos1InFrame =
                                                                    Point3d.placeIn frame pos1New
                                                            in
                                                            addFlatTexturedFaces frame vertexData remainingElementVertices remainingFaces pos0InFrame pos1InFrame uv0New uv1New outVertices outIdx outFaceIndices

                                                        Nothing ->
                                                            addFlatTexturedFaces frame vertexData [] remainingFaces pos0 prevPos uv0 prevUV outVertices outIdx outFaceIndices

                                                Nothing ->
                                                    addFlatTexturedFaces frame vertexData [] remainingFaces pos0 prevPos uv0 prevUV outVertices outIdx outFaceIndices

                                        Nothing ->
                                            addFlatTexturedFaces frame vertexData [] remainingFaces pos0 prevPos uv0 prevUV outVertices outIdx outFaceIndices

                                Nothing ->
                                    addFlatTexturedFaces frame vertexData [] remainingFaces pos0 prevPos uv0 prevUV outVertices outIdx outFaceIndices

                        _ ->
                            addFlatTexturedFaces frame vertexData [] remainingFaces pos0 prevPos uv0 prevUV outVertices outIdx outFaceIndices

                [] ->
                    ( outVertices, outFaceIndices )
