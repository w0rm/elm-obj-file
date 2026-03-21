module Obj.Internal.Triangles exposing
    ( texturedTriangles
    , triangles
    )

import Array exposing (Array)
import Frame3d exposing (Frame3d)
import Length exposing (Meters)
import Obj.Internal.MeshHelpers exposing (buildMeshResult, groupIndices, lookup1)
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
import TriangularMesh exposing (TriangularMesh)


triangles :
    Frame3d Meters coordinates { defines : ObjCoordinates }
    -> VertexData
    -> List String
    -> List Group
    -> Result String (TriangularMesh (Point3d Meters coordinates))
triangles frame vertexData filters groups =
    triangularMesh (addTriangles frame vertexData)
        filters
        groups
        -1
        vertexData.indexMap
        []
        []


texturedTriangles :
    Frame3d Meters coordinates { defines : ObjCoordinates }
    -> VertexData
    -> List String
    -> List Group
    -> Result String (TriangularMesh { position : Point3d Meters coordinates, uv : ( Float, Float ) })
texturedTriangles frame vertexData filters groups =
    triangularMesh (addTexturedTriangles frame vertexData)
        filters
        groups
        -1
        vertexData.indexMap
        []
        []


type alias IndexedTriangles a =
    { maxIndex : Int
    , indexMap : Array (List Int)
    , vertices : List a
    , faceIndices : List ( Int, Int, Int )
    }


type alias AddIndexedTriangles a =
    Int
    -> List Vertex
    -> List FaceElement
    -> Int
    -> Array (List Int)
    -> List a
    -> List Int
    -> List ( Int, Int, Int )
    -> Result String (IndexedTriangles a)


triangularMesh : AddIndexedTriangles a -> List String -> List Group -> Int -> Array (List Int) -> List a -> List ( Int, Int, Int ) -> Result String (TriangularMesh a)
triangularMesh add filters groups maxIndex indexMap vertices faceIndices =
    case groups of
        (Group _ ((FaceElement lineno _ elementVertices) :: faceElements) _ _) :: remainingElementGroups ->
            case add lineno elementVertices faceElements maxIndex indexMap vertices [] faceIndices of
                Ok newState ->
                    triangularMesh add filters remainingElementGroups newState.maxIndex newState.indexMap newState.vertices newState.faceIndices

                Err error ->
                    Err error

        (Group _ [] _ _) :: remainingElementGroups ->
            -- skip an empty group
            triangularMesh add filters remainingElementGroups maxIndex indexMap vertices faceIndices

        [] ->
            buildMeshResult filters (Array.fromList (List.reverse vertices)) faceIndices


addTriangles : Frame3d Meters coordinates { defines : ObjCoordinates } -> VertexData -> AddIndexedTriangles (Point3d Meters coordinates)
addTriangles frame vertexData lineno elementVertices elements maxIndex indexMap vertices indices faceIndices =
    case elementVertices of
        { p } :: remainingVertices ->
            case Array.get p indexMap of
                Just [ idx ] ->
                    addTriangles frame
                        vertexData
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
                            addTriangles frame
                                vertexData
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
                (FaceElement newLineno _ newElementVertices) :: remainingElements ->
                    addTriangles frame
                        vertexData
                        newLineno
                        newElementVertices
                        remainingElements
                        maxIndex
                        indexMap
                        vertices
                        []
                        newFaceIndices

                [] ->
                    Ok
                        { maxIndex = maxIndex
                        , indexMap = indexMap
                        , vertices = vertices
                        , faceIndices = newFaceIndices
                        }


addTexturedTriangles : Frame3d Meters coordinates { defines : ObjCoordinates } -> VertexData -> AddIndexedTriangles { position : Point3d Meters coordinates, uv : ( Float, Float ) }
addTexturedTriangles frame vertexData lineno elementVertices elements maxIndex indexMap vertices indices faceIndices =
    case elementVertices of
        { p, uv } :: remainingVertices ->
            if uv > -1 then
                let
                    lookupArray =
                        case Array.get p indexMap of
                            Just arr ->
                                arr

                            Nothing ->
                                []

                    idx =
                        lookup1 uv lookupArray
                in
                if idx > -1 then
                    addTexturedTriangles frame
                        vertexData
                        lineno
                        remainingVertices
                        elements
                        maxIndex
                        indexMap
                        vertices
                        (idx :: indices)
                        faceIndices

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
                                        (Array.set p (uv :: maxIndex + 1 :: lookupArray) indexMap)
                                        ({ position = Point3d.placeIn frame position
                                         , uv = uvCoord
                                         }
                                            :: vertices
                                        )
                                        (maxIndex + 1 :: indices)
                                        faceIndices

                                Nothing ->
                                    formatError lineno "Index out of range"

                        Nothing ->
                            formatError lineno "Index out of range"

            else
                formatError lineno "Vertex has no texture coordinates"

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
                (FaceElement newLineno _ newElementVertices) :: remainingElements ->
                    addTexturedTriangles frame
                        vertexData
                        newLineno
                        newElementVertices
                        remainingElements
                        maxIndex
                        indexMap
                        vertices
                        []
                        newFaceIndices

                [] ->
                    Ok
                        { maxIndex = maxIndex
                        , indexMap = indexMap
                        , vertices = vertices
                        , faceIndices = newFaceIndices
                        }
