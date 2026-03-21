module Obj.Internal.SmoothNormals exposing
    ( SmoothNormals
    , bitflag
    , exact
    , get
    )

import Array
import Bitwise
import Dict exposing (Dict)
import Obj.Internal.Parse
    exposing
        ( FaceElement(..)
        , Group(..)
        , ObjCoordinates
        , Vertex
        , VertexData
        )
import Point3d
import Quantity exposing (Unitless)
import Set
import Vector3d exposing (Vector3d)


type SmoothNormals
    = SmoothNormals (Int -> List ( Int, Vector3d Unitless ObjCoordinates ) -> Maybe (Vector3d Unitless ObjCoordinates) -> Maybe (Vector3d Unitless ObjCoordinates)) (Dict Int (List ( Int, Vector3d Unitless ObjCoordinates )))


exact : VertexData -> List Group -> SmoothNormals
exact vertexData groups =
    let
        smoothingGroupsSet =
            Set.fromList (smoothingGroupsHelp groups [])
    in
    SmoothNormals getExact (collectSmoothNormalsHelp (\smoothingGroup -> Set.member smoothingGroup smoothingGroupsSet) vertexData groups Dict.empty)


bitflag : VertexData -> List Group -> SmoothNormals
bitflag vertexData groups =
    let
        allBits =
            List.foldl Bitwise.or 0 (smoothingGroupsHelp groups [])
    in
    SmoothNormals getBitflag (collectSmoothNormalsHelp (\smoothingGroup -> Bitwise.and smoothingGroup allBits /= 0) vertexData groups Dict.empty)


smoothingGroupsHelp : List Group -> List Int -> List Int
smoothingGroupsHelp groups outSmoothingGroups =
    case groups of
        [] ->
            outSmoothingGroups

        (Group record _ _ _) :: remainingGroups ->
            if record.smoothingGroup == 0 then
                smoothingGroupsHelp remainingGroups outSmoothingGroups

            else
                smoothingGroupsHelp remainingGroups (record.smoothingGroup :: outSmoothingGroups)


collectSmoothNormalsHelp : (Int -> Bool) -> VertexData -> List Group -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates )) -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
collectSmoothNormalsHelp matches vertexData groups outSmoothNormals =
    case groups of
        [] ->
            outSmoothNormals

        (Group { smoothingGroup } faceElements _ _) :: remainingGroups ->
            if matches smoothingGroup then
                collectSmoothNormalsHelp matches
                    vertexData
                    remainingGroups
                    (collectSmoothNormalsFaces vertexData smoothingGroup faceElements outSmoothNormals)

            else
                collectSmoothNormalsHelp matches vertexData remainingGroups outSmoothNormals


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


collectSmoothNormalsFaces : VertexData -> Int -> List FaceElement -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates )) -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
collectSmoothNormalsFaces vertexData smoothingGroup faceElements outSmoothNormals =
    case faceElements of
        [] ->
            outSmoothNormals

        (FaceElement _ _ ((v0 :: v1 :: ((_ :: _) as remainingElementVertices)) as elementVertices)) :: remainingFaceElements ->
            case Array.get v0.p vertexData.positions of
                Just pos0 ->
                    case Array.get v1.p vertexData.positions of
                        Just pos1 ->
                            let
                                normal =
                                    polygonFanNormal vertexData (Point3d.toMeters pos0) (Point3d.toMeters pos1) remainingElementVertices Vector3d.zero
                            in
                            collectSmoothNormalsFaces vertexData
                                smoothingGroup
                                remainingFaceElements
                                (collectSmoothNormalsVertices smoothingGroup normal elementVertices outSmoothNormals)

                        Nothing ->
                            collectSmoothNormalsFaces vertexData smoothingGroup remainingFaceElements outSmoothNormals

                Nothing ->
                    collectSmoothNormalsFaces vertexData smoothingGroup remainingFaceElements outSmoothNormals

        (FaceElement _ _ _) :: remainingFaceElements ->
            collectSmoothNormalsFaces vertexData smoothingGroup remainingFaceElements outSmoothNormals


get : Int -> Int -> SmoothNormals -> Maybe (Vector3d Unitless ObjCoordinates)
get p smoothingGroup (SmoothNormals lookup dict) =
    case Dict.get p dict of
        Nothing ->
            Nothing

        Just entries ->
            lookup smoothingGroup entries Nothing


getExact : Int -> List ( Int, Vector3d Unitless ObjCoordinates ) -> Maybe (Vector3d Unitless ObjCoordinates) -> Maybe (Vector3d Unitless ObjCoordinates)
getExact smoothingGroup entries _ =
    case entries of
        [] ->
            Nothing

        ( currentSmoothingGroup, currentNormal ) :: remainingEntries ->
            if smoothingGroup == currentSmoothingGroup then
                Just (Vector3d.normalize currentNormal)

            else
                getExact smoothingGroup remainingEntries Nothing


{-| Unlike `getExact`, a position can have entries for several distinct
smoothing groups — e.g. `[ ( 1, n1 ), ( 2, n2 ) ]` — and a bitflag query like
`3` matches both (3 & 1 /= 0, 3 & 2 /= 0), so they genuinely need to be summed
here. `addNormal` only merges entries with the _exact same_ group integer, so
groups 1 and 2 remain separate in the dict and the summation happens at lookup
time.
-}
getBitflag : Int -> List ( Int, Vector3d Unitless ObjCoordinates ) -> Maybe (Vector3d Unitless ObjCoordinates) -> Maybe (Vector3d Unitless ObjCoordinates)
getBitflag smoothingGroup entries outNormal =
    case entries of
        [] ->
            case outNormal of
                Just normal ->
                    Just (Vector3d.normalize normal)

                Nothing ->
                    Nothing

        ( currentSmoothingGroup, currentNormal ) :: remainingEntries ->
            getBitflag
                smoothingGroup
                remainingEntries
                (if Bitwise.and smoothingGroup currentSmoothingGroup /= 0 then
                    case outNormal of
                        Nothing ->
                            Just currentNormal

                        Just existing ->
                            Just (Vector3d.plus existing currentNormal)

                 else
                    outNormal
                )


{-| Add `normal` to the entry for `smoothingGroup` in `entries`, or prepend a new entry if none exists.
Order within the list is not significant; `get` scans the whole list.
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


collectSmoothNormalsVertices : Int -> Vector3d Unitless ObjCoordinates -> List Vertex -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates )) -> Dict Int (List ( Int, Vector3d Unitless ObjCoordinates ))
collectSmoothNormalsVertices smoothingGroup normal elementVertices outSmoothNormals =
    case elementVertices of
        [] ->
            outSmoothNormals

        { p } :: remainingElementVertices ->
            -- Key by position: p alone is sufficient when each position belongs to one group,
            -- but the same geometric position can appear in multiple smoothing groups and each
            -- group needs its own normal, so smoothingGroup is stored in the inner list.
            collectSmoothNormalsVertices smoothingGroup
                normal
                remainingElementVertices
                (Dict.insert p
                    (case Dict.get p outSmoothNormals of
                        Just entries ->
                            addNormal smoothingGroup normal entries []

                        Nothing ->
                            [ ( smoothingGroup, normal ) ]
                    )
                    outSmoothNormals
                )
