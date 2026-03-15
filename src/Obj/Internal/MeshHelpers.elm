module Obj.Internal.MeshHelpers exposing
    ( buildMeshResult
    , groupIndices
    , lookup1
    , lookup2
    )

import Array exposing (Array)
import TriangularMesh exposing (TriangularMesh)


{-| Build the final `TriangularMesh` or return an error if no faces were produced.
-}
buildMeshResult : List String -> Array v -> List ( Int, Int, Int ) -> Result String (TriangularMesh v)
buildMeshResult filters vertices faceIndices =
    case faceIndices of
        _ :: _ ->
            Ok (TriangularMesh.indexed vertices faceIndices)

        [] ->
            case filters of
                _ :: _ ->
                    Err ("No faces found for " ++ String.join ", " filters)

                [] ->
                    Err "No faces found"


{-| Split the indices of the faces into triples, forming a triangle fan.
-}
groupIndices : Int -> List Int -> List ( Int, Int, Int ) -> List ( Int, Int, Int )
groupIndices p1 more result =
    case more of
        p2 :: rest ->
            case rest of
                p3 :: _ ->
                    -- Note that when it comes to grouping, the order of points is reversed
                    -- but the indices were reversed too, when parsing, so this is fine :-)
                    groupIndices p1 rest (( p1, p2, p3 ) :: result)

                [] ->
                    result

        [] ->
            result


{-| returns -1 if not found
-}
lookup1 : Int -> List Int -> Int
lookup1 idx1 list =
    case list of
        i1 :: result :: rest ->
            if idx1 - i1 == 0 then
                result

            else
                lookup1 idx1 rest

        _ ->
            -1


{-| returns -1 if not found
-}
lookup2 : Int -> Int -> List Int -> Int
lookup2 idx1 idx2 list =
    case list of
        i1 :: i2 :: result :: rest ->
            if idx1 - i1 == 0 && idx2 - i2 == 0 then
                result

            else
                lookup2 idx1 idx2 rest

        _ ->
            -1
