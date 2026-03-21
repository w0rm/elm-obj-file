module Obj.Internal.SmoothNormals exposing
    ( SmoothNormals
    , bitflag
    , exact
    , get
    )

import Array exposing (Array)
import Bitwise
import Length exposing (Meters)
import Obj.Internal.Parse
    exposing
        ( FaceElement(..)
        , Group(..)
        , Vertex
        )
import Point3d exposing (Point3d)
import Quantity exposing (Unitless)
import Set
import Vector3d exposing (Vector3d)


type SmoothNormals coordinates
    = SmoothNormals (Int -> List ( Int, Vector3d Unitless coordinates ) -> Maybe (Vector3d Unitless coordinates) -> Maybe (Vector3d Unitless coordinates)) (Array (List ( Int, Vector3d Unitless coordinates )))


exact : Array (Point3d Meters coordinates) -> List Group -> SmoothNormals coordinates
exact positions groups =
    let
        smoothingGroupsSet =
            Set.fromList (smoothingGroupsHelp groups [])
    in
    SmoothNormals getExact (collectSmoothNormalsHelp (\smoothingGroup -> Set.member smoothingGroup smoothingGroupsSet) positions groups (Array.repeat (Array.length positions) []))


bitflag : Array (Point3d Meters coordinates) -> List Group -> SmoothNormals coordinates
bitflag positions groups =
    let
        allBits =
            List.foldl Bitwise.or 0 (smoothingGroupsHelp groups [])
    in
    SmoothNormals getBitflag (collectSmoothNormalsHelp (\smoothingGroup -> Bitwise.and smoothingGroup allBits /= 0) positions groups (Array.repeat (Array.length positions) []))


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


collectSmoothNormalsHelp : (Int -> Bool) -> Array (Point3d Meters coordinates) -> List Group -> Array (List ( Int, Vector3d Unitless coordinates )) -> Array (List ( Int, Vector3d Unitless coordinates ))
collectSmoothNormalsHelp matches positions groups outSmoothNormals =
    case groups of
        [] ->
            outSmoothNormals

        (Group { smoothingGroup } faceElements _ _) :: remainingGroups ->
            if matches smoothingGroup then
                collectSmoothNormalsHelp matches
                    positions
                    remainingGroups
                    (collectSmoothNormalsFaces positions smoothingGroup faceElements outSmoothNormals)

            else
                collectSmoothNormalsHelp matches positions remainingGroups outSmoothNormals


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
    Array (Point3d Meters coordinates)
    -> { x : Float, y : Float, z : Float }
    -> { x : Float, y : Float, z : Float }
    -> List Vertex
    -> Vector3d Unitless coordinates
    -> Vector3d Unitless coordinates
polygonFanNormal positions p0 prevPos elementVertices normal =
    case elementVertices of
        vB :: remainingElementVertices ->
            case Array.get vB.p positions of
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
                    polygonFanNormal positions
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
                    polygonFanNormal positions p0 prevPos remainingElementVertices normal

        _ ->
            normal


collectSmoothNormalsFaces : Array (Point3d Meters coordinates) -> Int -> List FaceElement -> Array (List ( Int, Vector3d Unitless coordinates )) -> Array (List ( Int, Vector3d Unitless coordinates ))
collectSmoothNormalsFaces positions smoothingGroup faceElements outSmoothNormals =
    case faceElements of
        [] ->
            outSmoothNormals

        (FaceElement _ _ ((v0 :: v1 :: ((_ :: _) as remainingElementVertices)) as elementVertices)) :: remainingFaceElements ->
            case Array.get v0.p positions of
                Just pos0 ->
                    case Array.get v1.p positions of
                        Just pos1 ->
                            let
                                normal =
                                    polygonFanNormal positions (Point3d.toMeters pos0) (Point3d.toMeters pos1) remainingElementVertices Vector3d.zero
                            in
                            collectSmoothNormalsFaces positions
                                smoothingGroup
                                remainingFaceElements
                                (collectSmoothNormalsVertices smoothingGroup normal elementVertices outSmoothNormals)

                        Nothing ->
                            collectSmoothNormalsFaces positions smoothingGroup remainingFaceElements outSmoothNormals

                Nothing ->
                    collectSmoothNormalsFaces positions smoothingGroup remainingFaceElements outSmoothNormals

        (FaceElement _ _ _) :: remainingFaceElements ->
            collectSmoothNormalsFaces positions smoothingGroup remainingFaceElements outSmoothNormals


get : Int -> Int -> SmoothNormals coordinates -> Maybe (Vector3d Unitless coordinates)
get p smoothingGroup (SmoothNormals lookup arr) =
    case Array.get p arr of
        Nothing ->
            Nothing

        Just entries ->
            lookup smoothingGroup entries Nothing


getExact : Int -> List ( Int, Vector3d Unitless coordinates ) -> Maybe (Vector3d Unitless coordinates) -> Maybe (Vector3d Unitless coordinates)
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
getBitflag : Int -> List ( Int, Vector3d Unitless coordinates ) -> Maybe (Vector3d Unitless coordinates) -> Maybe (Vector3d Unitless coordinates)
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
addNormal : Int -> Vector3d Unitless coordinates -> List ( Int, Vector3d Unitless coordinates ) -> List ( Int, Vector3d Unitless coordinates ) -> List ( Int, Vector3d Unitless coordinates )
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


collectSmoothNormalsVertices : Int -> Vector3d Unitless coordinates -> List Vertex -> Array (List ( Int, Vector3d Unitless coordinates )) -> Array (List ( Int, Vector3d Unitless coordinates ))
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
                (Array.set p
                    (case Array.get p outSmoothNormals of
                        Just entries ->
                            addNormal smoothingGroup normal entries []

                        Nothing ->
                            [ ( smoothingGroup, normal ) ]
                    )
                    outSmoothNormals
                )
