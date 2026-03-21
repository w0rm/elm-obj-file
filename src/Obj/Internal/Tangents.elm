module Obj.Internal.Tangents exposing (compute)

import Array exposing (Array)
import Length exposing (Meters)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..), Unitless)
import Vector3d exposing (Vector3d)


compute :
    List { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ) }
    -> List ( Int, Int, Int )
    -> Array { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ), tangent : Vector3d Unitless coordinates, tangentBasisIsRightHanded : Bool }
compute faceVertices faceIndices =
    Array.map
        (\{ tangent, position, normal, uv, bitangent } ->
            let
                (Quantity dot) =
                    Vector3d.dot tangent normal

                -- Gram-Schmidt: subtract the component of the accumulated
                -- tangent that points along the normal (T - (N·T)N),
                -- leaving a tangent that lies flat on the surface,
                -- then normalize to unit length.
                normalizedTangent =
                    Vector3d.normalize (Vector3d.minus (Vector3d.scaleBy dot normal) tangent)

                (Quantity handednessDot) =
                    Vector3d.dot (Vector3d.cross normal tangent) bitangent
            in
            { position = position
            , uv = uv
            , tangent = normalizedTangent
            , normal = normal
            , tangentBasisIsRightHanded = handednessDot > 0
            }
        )
        (computeHelp faceIndices (Array.fromList (addZeroTangents faceVertices [])))


{-| Adds tangents to the vertices, initializing them to zero. This is necessary for the tangent accumulation.
Note: this reverses the order of vertices.
-}
addZeroTangents :
    List { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ) }
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


computeHelp :
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
computeHelp faceIndices faceVertices =
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
                                    computeHelp remainingFaceIndices
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
                                    computeHelp remainingFaceIndices faceVertices

                        Nothing ->
                            computeHelp remainingFaceIndices faceVertices

                Nothing ->
                    computeHelp remainingFaceIndices faceVertices
