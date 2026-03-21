module Obj.Internal.Triangles exposing
    ( texturedTriangles
    , triangles
    )

import Array
import Frame3d exposing (Frame3d)
import Length exposing (Meters)
import Obj.Internal.IndexMap as IndexMap exposing (IndexMap, Key1, Key2)
import Obj.Internal.MeshHelpers exposing (buildMeshResult, groupIndices)
import Obj.Internal.Parse
    exposing
        ( FaceElement(..)
        , Group(..)
        , Vertex
        , VertexData
        , formatError
        )
import Point3d exposing (Point3d)
import TriangularMesh exposing (TriangularMesh)


triangles :
    Frame3d Meters coordinates { defines : objCoordinates }
    -> VertexData objCoordinates
    -> List String
    -> List Group
    -> Result String (TriangularMesh (Point3d Meters coordinates))
triangles frame vertexData filters groups =
    triangularMesh (addTriangles frame vertexData)
        filters
        groups
        -1
        (IndexMap.init vertexData.emptyIndexMap)
        []
        []


texturedTriangles :
    Frame3d Meters coordinates { defines : objCoordinates }
    -> VertexData objCoordinates
    -> List String
    -> List Group
    -> Result String (TriangularMesh { position : Point3d Meters coordinates, uv : ( Float, Float ) })
texturedTriangles frame vertexData filters groups =
    triangularMesh (addTexturedTriangles frame vertexData)
        filters
        groups
        -1
        (IndexMap.init2 vertexData.emptyIndexMap)
        []
        []


type alias IndexedTriangles a k =
    { maxIndex : Int
    , indexMap : IndexMap k
    , faceVertices : List a
    , faceIndices : List ( Int, Int, Int )
    }


type alias AddIndexedTriangles a k =
    Int
    -> List Vertex
    -> List FaceElement
    -> Int
    -> IndexMap k
    -> List a
    -> List Int
    -> List ( Int, Int, Int )
    -> Result String (IndexedTriangles a k)


triangularMesh : AddIndexedTriangles a k -> List String -> List Group -> Int -> IndexMap k -> List a -> List ( Int, Int, Int ) -> Result String (TriangularMesh a)
triangularMesh add filters groups maxIndex indexMap outVertices outFaceIndices =
    case groups of
        (Group _ ((FaceElement lineno _ elementVertices) :: remainingFaceElements) _ _) :: remainingElementGroups ->
            case add lineno elementVertices remainingFaceElements maxIndex indexMap outVertices [] outFaceIndices of
                Ok newState ->
                    triangularMesh add filters remainingElementGroups newState.maxIndex newState.indexMap newState.faceVertices newState.faceIndices

                Err error ->
                    Err error

        (Group _ [] _ _) :: remainingElementGroups ->
            -- skip an empty group
            triangularMesh add filters remainingElementGroups maxIndex indexMap outVertices outFaceIndices

        [] ->
            buildMeshResult filters (Array.fromList (List.reverse outVertices)) outFaceIndices


addTriangles : Frame3d Meters coordinates { defines : objCoordinates } -> VertexData objCoordinates -> AddIndexedTriangles (Point3d Meters coordinates) Key1
addTriangles frame vertexData lineno elementVertices elements maxIndex indexMap outVertices outIndices outFaceIndices =
    case elementVertices of
        { p } :: remainingVertices ->
            let
                idx =
                    IndexMap.get p indexMap
            in
            if idx > -1 then
                addTriangles frame
                    vertexData
                    lineno
                    remainingVertices
                    elements
                    maxIndex
                    indexMap
                    outVertices
                    (idx :: outIndices)
                    outFaceIndices

            else
                case Array.get p vertexData.positions of
                    Just vertex ->
                        addTriangles frame
                            vertexData
                            lineno
                            remainingVertices
                            elements
                            (maxIndex + 1)
                            (IndexMap.insert p (maxIndex + 1) indexMap)
                            (Point3d.placeIn frame vertex :: outVertices)
                            (maxIndex + 1 :: outIndices)
                            outFaceIndices

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
                (FaceElement newLineno _ newElementVertices) :: remainingElements ->
                    addTriangles frame
                        vertexData
                        newLineno
                        newElementVertices
                        remainingElements
                        maxIndex
                        indexMap
                        outVertices
                        []
                        newFaceIndices

                [] ->
                    Ok
                        { maxIndex = maxIndex
                        , indexMap = indexMap
                        , faceVertices = outVertices
                        , faceIndices = newFaceIndices
                        }


addTexturedTriangles : Frame3d Meters coordinates { defines : objCoordinates } -> VertexData objCoordinates -> AddIndexedTriangles { position : Point3d Meters coordinates, uv : ( Float, Float ) } Key2
addTexturedTriangles frame vertexData lineno elementVertices elements maxIndex indexMap outVertices outIndices outFaceIndices =
    case elementVertices of
        { p, uv } :: remainingVertices ->
            if uv > -1 then
                let
                    idx =
                        IndexMap.get2 p uv indexMap
                in
                if idx > -1 then
                    addTexturedTriangles frame
                        vertexData
                        lineno
                        remainingVertices
                        elements
                        maxIndex
                        indexMap
                        outVertices
                        (idx :: outIndices)
                        outFaceIndices

                else
                    case Array.get p vertexData.positions of
                        Just position ->
                            case Array.get uv vertexData.uvs of
                                Just uvCoord ->
                                    addTexturedTriangles frame
                                        vertexData
                                        lineno
                                        remainingVertices
                                        elements
                                        (maxIndex + 1)
                                        (IndexMap.insert2 p uv (maxIndex + 1) indexMap)
                                        ({ position = Point3d.placeIn frame position
                                         , uv = uvCoord
                                         }
                                            :: outVertices
                                        )
                                        (maxIndex + 1 :: outIndices)
                                        outFaceIndices

                                Nothing ->
                                    formatError lineno "Index out of range"

                        Nothing ->
                            formatError lineno "Index out of range"

            else
                formatError lineno "Vertex has no texture coordinates"

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
                (FaceElement newLineno _ newElementVertices) :: remainingElements ->
                    addTexturedTriangles frame
                        vertexData
                        newLineno
                        newElementVertices
                        remainingElements
                        maxIndex
                        indexMap
                        outVertices
                        []
                        newFaceIndices

                [] ->
                    Ok
                        { maxIndex = maxIndex
                        , indexMap = indexMap
                        , faceVertices = outVertices
                        , faceIndices = newFaceIndices
                        }
