module Obj.Internal.MeshHelpers exposing
    ( buildMeshResult
    , groupIndices
    )

import Array exposing (Array)
import TriangularMesh exposing (TriangularMesh)


{-| Build the final `TriangularMesh` or return an error if no faces were produced.
-}
buildMeshResult : List String -> Array v -> List ( Int, Int, Int ) -> Result String (TriangularMesh v)
buildMeshResult filters faceVertices faceIndices =
    case faceIndices of
        _ :: _ ->
            Ok (TriangularMesh.indexed faceVertices faceIndices)

        [] ->
            case filters of
                _ :: _ ->
                    Err ("No faces found for " ++ String.join ", " filters)

                [] ->
                    Err "No faces found"


{-| Split the indices of the faces into triples, forming a triangle fan.
-}
groupIndices : Int -> List Int -> List ( Int, Int, Int ) -> List ( Int, Int, Int )
groupIndices p1 indices outFaceIndices =
    case indices of
        p2 :: rest ->
            case rest of
                p3 :: _ ->
                    -- Note that when it comes to grouping, the order of points is reversed
                    -- but the indices were reversed too, when parsing, so this is fine :-)
                    groupIndices p1 rest (( p1, p2, p3 ) :: outFaceIndices)

                [] ->
                    outFaceIndices

        [] ->
            outFaceIndices
