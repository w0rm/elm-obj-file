module SmoothingGroups exposing
    ( bumpyFaces
    , faces
    , texturedFaces
    )

import Array
import Expect
import Length
import Obj.Decode as Decode
import Quantity exposing (Quantity(..))
import Test exposing (Test)
import TriangularMesh
import Vector3d


faces : Test
faces =
    Test.describe "faces with smoothing groups"
        [ Test.test "s off: flat shading normals point outward (+Z for XY-plane triangle)" <|
            \_ ->
                -- Triangle (0,0,0),(1,0,0),(0,1,0) is CCW when viewed from +Z,
                -- so computed normals must have a positive Z component.
                xyPlaneObj
                    |> Decode.decodeString Length.centimeters Decode.faces
                    |> Result.map
                        (\mesh ->
                            TriangularMesh.vertices mesh
                                |> Array.toList
                                |> List.all
                                    (\v ->
                                        let
                                            (Quantity z) =
                                                Vector3d.zComponent v.normal
                                        in
                                        z > 0
                                    )
                        )
                    |> Expect.equal (Ok True)
        , Test.test "s 1: smooth normals point outward (+Z for XY-plane triangle)" <|
            \_ ->
                xyPlaneSmoothObj
                    |> Decode.decodeString Length.centimeters Decode.faces
                    |> Result.map
                        (\mesh ->
                            TriangularMesh.vertices mesh
                                |> Array.toList
                                |> List.all
                                    (\v ->
                                        let
                                            (Quantity z) =
                                                Vector3d.zComponent v.normal
                                        in
                                        z > 0
                                    )
                        )
                    |> Expect.equal (Ok True)
        , Test.test "s off: flat shading produces 3 unshared vertices per triangle" <|
            \_ ->
                sOffObj
                    |> Decode.decodeString Length.centimeters Decode.faces
                    |> Result.map
                        (\mesh ->
                            ( Array.length (TriangularMesh.vertices mesh)
                            , List.length (TriangularMesh.faceIndices mesh)
                            )
                        )
                    |> Expect.equal (Ok ( 6, 2 ))
        , Test.test "s off: all reconstructed normals have unit length" <|
            \_ ->
                sOffObj
                    |> Decode.decodeString Length.centimeters Decode.faces
                    |> Result.map
                        (\mesh ->
                            TriangularMesh.vertices mesh
                                |> Array.toList
                                |> List.all
                                    (\v ->
                                        let
                                            (Quantity len) =
                                                Vector3d.length v.normal
                                        in
                                        abs (len - 1) < 0.0001
                                    )
                        )
                    |> Expect.equal (Ok True)
        , Test.test "s 1: shares vertices for smooth normals" <|
            \_ ->
                s1SmoothObj
                    |> Decode.decodeString Length.centimeters Decode.faces
                    |> Result.map
                        (\mesh ->
                            ( Array.length (TriangularMesh.vertices mesh)
                            , List.length (TriangularMesh.faceIndices mesh)
                            )
                        )
                    |> Expect.equal (Ok ( 4, 2 ))
        , Test.test "s 1: all reconstructed normals have unit length" <|
            \_ ->
                s1SmoothObj
                    |> Decode.decodeString Length.centimeters Decode.faces
                    |> Result.map
                        (\mesh ->
                            TriangularMesh.vertices mesh
                                |> Array.toList
                                |> List.all
                                    (\v ->
                                        let
                                            (Quantity len) =
                                                Vector3d.length v.normal
                                        in
                                        abs (len - 1) < 0.0001
                                    )
                        )
                    |> Expect.equal (Ok True)
        , Test.test "mixed s groups: vertex splitting at hard edges" <|
            \_ ->
                mixedSGroupsObj
                    |> Decode.decodeString Length.centimeters Decode.faces
                    |> Result.map
                        (\mesh ->
                            ( Array.length (TriangularMesh.vertices mesh)
                            , List.length (TriangularMesh.faceIndices mesh)
                            )
                        )
                    |> Expect.equal (Ok ( 6, 2 ))
        , Test.test "mixed: fast-path faces with vn and reconstructed faces coexist" <|
            \_ ->
                -- First face has explicit normals (fast path), second has none (reconstruction)
                mixedVnAndNoVnObj
                    |> Decode.decodeString Length.centimeters Decode.faces
                    |> Result.map
                        (\mesh ->
                            ( Array.length (TriangularMesh.vertices mesh)
                            , List.length (TriangularMesh.faceIndices mesh)
                            )
                        )
                    |> Expect.equal (Ok ( 6, 2 ))
        , Test.test "bitflag: s 1 and s 2 (Blender bitflag export, all powers of 2) produce hard edge" <|
            \_ ->
                -- s 1 (01) and s 2 (10) share no bits, so should split into 6 vertices.
                -- All values are powers of 2, so bitflag mode is auto-detected.
                mixedSGroupsObj
                    |> Decode.decodeString Length.centimeters Decode.faces
                    |> Result.map
                        (\mesh ->
                            ( Array.length (TriangularMesh.vertices mesh)
                            , List.length (TriangularMesh.faceIndices mesh)
                            )
                        )
                    |> Expect.equal (Ok ( 6, 2 ))
        , Test.test "integer mode: s 1 and s 3 are different IDs, produce hard edge" <|
            \_ ->
                -- s 1 and s 3 share bit 0 in binary, but integer (equality) mode is used:
                -- 1 /= 3 → vertex splitting → 6 vertices.
                integerModeNonPow2Obj
                    |> Decode.decodeString Length.centimeters Decode.faces
                    |> Result.map
                        (\mesh ->
                            ( Array.length (TriangularMesh.vertices mesh)
                            , List.length (TriangularMesh.faceIndices mesh)
                            )
                        )
                    |> Expect.equal (Ok ( 6, 2 ))
        ]


texturedFaces : Test
texturedFaces =
    Test.describe "texturedFaces with smoothing groups"
        [ Test.test "s off: flat shading normals point outward (+Z for XY-plane triangle)" <|
            \_ ->
                xyPlaneTexturedObj
                    |> Decode.decodeString Length.centimeters Decode.texturedFaces
                    |> Result.map
                        (\mesh ->
                            TriangularMesh.vertices mesh
                                |> Array.toList
                                |> List.all
                                    (\v ->
                                        let
                                            (Quantity z) =
                                                Vector3d.zComponent v.normal
                                        in
                                        z > 0
                                    )
                        )
                    |> Expect.equal (Ok True)
        , Test.test "s 1: smooth normals point outward (+Z for XY-plane triangle)" <|
            \_ ->
                xyPlaneSmoothTexturedObj
                    |> Decode.decodeString Length.centimeters Decode.texturedFaces
                    |> Result.map
                        (\mesh ->
                            TriangularMesh.vertices mesh
                                |> Array.toList
                                |> List.all
                                    (\v ->
                                        let
                                            (Quantity z) =
                                                Vector3d.zComponent v.normal
                                        in
                                        z > 0
                                    )
                        )
                    |> Expect.equal (Ok True)
        , Test.test "s off: flat shading produces 3 unshared vertices per triangle" <|
            \_ ->
                sOffTexturedObj
                    |> Decode.decodeString Length.centimeters Decode.texturedFaces
                    |> Result.map
                        (\mesh ->
                            ( Array.length (TriangularMesh.vertices mesh)
                            , List.length (TriangularMesh.faceIndices mesh)
                            )
                        )
                    |> Expect.equal (Ok ( 6, 2 ))
        , Test.test "s 1: shares vertices for smooth normals" <|
            \_ ->
                s1SmoothTexturedObj
                    |> Decode.decodeString Length.centimeters Decode.texturedFaces
                    |> Result.map
                        (\mesh ->
                            ( Array.length (TriangularMesh.vertices mesh)
                            , List.length (TriangularMesh.faceIndices mesh)
                            )
                        )
                    |> Expect.equal (Ok ( 4, 2 ))
        , Test.test "s 1: quad contributes full area weight to smooth normal at shared vertex" <|
            \_ ->
                -- V0 is shared between a triangle (XY plane, normal -Z, area 2) and a
                -- quad (XZ plane, normal +Y, area 4). With correct area weighting the quad
                -- contributes twice as much at V0, so |Y| > |Z| there. The bug counted
                -- only the first fan triangle of the quad (area 2), making both faces equal
                -- weight and |Y| = |Z|. We identify V0 uniquely by its UV (0,0).
                quadTriangleSmoothTexturedObj
                    |> Decode.decodeString Length.centimeters Decode.texturedFaces
                    |> Result.map
                        (\mesh ->
                            TriangularMesh.vertices mesh
                                |> Array.toList
                                |> List.filterMap
                                    (\v ->
                                        if v.uv == ( 0.0, 0.0 ) then
                                            let
                                                (Quantity y) =
                                                    Vector3d.yComponent v.normal

                                                (Quantity z) =
                                                    Vector3d.zComponent v.normal
                                            in
                                            Just (abs y > abs z)

                                        else
                                            Nothing
                                    )
                        )
                    |> Expect.equal (Ok [ True ])
        , Test.test "mixed s groups: vertex splitting at hard edges" <|
            \_ ->
                mixedSGroupsTexturedObj
                    |> Decode.decodeString Length.centimeters Decode.texturedFaces
                    |> Result.map
                        (\mesh ->
                            ( Array.length (TriangularMesh.vertices mesh)
                            , List.length (TriangularMesh.faceIndices mesh)
                            )
                        )
                    |> Expect.equal (Ok ( 6, 2 ))
        , Test.test "s off: quad split into two triangles have consistent geometric winding" <|
            \_ ->
                -- The bug swapped pos0/posN in the recursive call of flatTexturedFacesNormals,
                -- reversing the winding of every triangle after the first in a polygon.
                -- Stored normals are reused from the first triangle so they don't catch it;
                -- instead, compute the geometric face normal from vertex positions and check
                -- that all faces are wound the same way (same Z sign).
                xyPlaneQuadTexturedObj
                    |> Decode.decodeString Length.centimeters Decode.texturedFaces
                    |> Result.map
                        (\mesh ->
                            let
                                zSigns =
                                    TriangularMesh.faceVertices mesh
                                        |> List.map
                                            (\( a, b, c ) ->
                                                let
                                                    (Quantity z) =
                                                        Vector3d.zComponent
                                                            (Vector3d.cross
                                                                (Vector3d.from a.position b.position)
                                                                (Vector3d.from a.position c.position)
                                                            )
                                                in
                                                z > 0
                                            )
                            in
                            case zSigns of
                                first :: rest ->
                                    List.all ((==) first) rest

                                [] ->
                                    False
                        )
                    |> Expect.equal (Ok True)
        ]


bumpyFaces : Test
bumpyFaces =
    Test.describe "bumpyFaces with smoothing groups"
        [ Test.test "s off: flat shading normals point outward (+Z for XY-plane triangle)" <|
            \_ ->
                xyPlaneTexturedObj
                    |> Decode.decodeString Length.centimeters Decode.bumpyFaces
                    |> Result.map
                        (\mesh ->
                            TriangularMesh.vertices mesh
                                |> Array.toList
                                |> List.all
                                    (\v ->
                                        let
                                            (Quantity z) =
                                                Vector3d.zComponent v.normal
                                        in
                                        z > 0
                                    )
                        )
                    |> Expect.equal (Ok True)
        , Test.test "s 1: smooth normals point outward (+Z for XY-plane triangle)" <|
            \_ ->
                xyPlaneSmoothTexturedObj
                    |> Decode.decodeString Length.centimeters Decode.bumpyFaces
                    |> Result.map
                        (\mesh ->
                            TriangularMesh.vertices mesh
                                |> Array.toList
                                |> List.all
                                    (\v ->
                                        let
                                            (Quantity z) =
                                                Vector3d.zComponent v.normal
                                        in
                                        z > 0
                                    )
                        )
                    |> Expect.equal (Ok True)
        , Test.test "s off: flat shading produces 3 unshared vertices per triangle" <|
            \_ ->
                sOffTexturedObj
                    |> Decode.decodeString Length.centimeters Decode.bumpyFaces
                    |> Result.map
                        (\mesh ->
                            ( Array.length (TriangularMesh.vertices mesh)
                            , List.length (TriangularMesh.faceIndices mesh)
                            )
                        )
                    |> Expect.equal (Ok ( 6, 2 ))
        , Test.test "s 1: shares vertices for smooth normals" <|
            \_ ->
                s1SmoothTexturedObj
                    |> Decode.decodeString Length.centimeters Decode.bumpyFaces
                    |> Result.map
                        (\mesh ->
                            ( Array.length (TriangularMesh.vertices mesh)
                            , List.length (TriangularMesh.faceIndices mesh)
                            )
                        )
                    |> Expect.equal (Ok ( 4, 2 ))
        ]


{-| Two triangles, s off, no explicit normals.
Each triangle gets 3 unshared vertices (flat shading).
-}
sOffObj : String
sOffObj =
    """v 0 0 0
v 1 0 0
v 1 1 0
v 0 1 0
s off
f 1 2 3
f 1 3 4"""


{-| Two co-planar triangles sharing an edge, s 1, no explicit normals.
Vertices 2 (pos 1,0,0) and 4 (pos 1,1,0) appear in both triangles,
so they should be deduplicated: 4 vertices total, not 6.
-}
s1SmoothObj : String
s1SmoothObj =
    """v 0 0 0
v 1 0 0
v 2 0 0
v 1 1 0
s 1
f 1 2 4
f 2 3 4"""


{-| Two triangles with different smoothing groups sharing positions 1 and 2.
Vertex splitting produces separate vertices per group: 6 vertices total.
-}
mixedSGroupsObj : String
mixedSGroupsObj =
    """v 0 0 0
v 1 0 0
v 0 1 0
v 0 0 1
s 1
f 1 2 3
s 2
f 1 2 4"""


{-| Two triangles with s 1 and s 3. In integer (equality) mode: 1 /= 3 → vertex splitting → 6 vertices.
-}
integerModeNonPow2Obj : String
integerModeNonPow2Obj =
    """v 0 0 0
v 1 0 0
v 0 1 0
v 0 0 1
s 1
f 1 2 3
s 3
f 1 2 4"""


{-| First face has explicit normals (fast path), second face has none (reconstruction, s off).
Both should be handled: 3 fast-path vertices + 3 reconstructed = 6 total.
-}
mixedVnAndNoVnObj : String
mixedVnAndNoVnObj =
    """v 0 0 0
v 1 0 0
v 0 1 0
v 0 0 1
vn 0 0 1
s off
f 1//1 2//1 3//1
f 1 2 4"""


{-| A triangle in the XY plane and a quad in the XZ plane sharing edge V0-V1, s 1.

V0=(0,0,0) and V1=(2,0,0) are shared; their smooth normal is the area-weighted
sum of the two face normals. The quad (area 4) is twice the triangle (area 2),
so |Y| should exceed |Z| at V0 when the quad's full area is counted.

-}
quadTriangleSmoothTexturedObj : String
quadTriangleSmoothTexturedObj =
    """v 0 0 0
v 2 0 0
v 0 2 0
v 2 0 2
v 0 0 2
vt 0 0
vt 1 0
vt 0 1
vt 1 1
vt 0.5 0.5
s 1
f 1/1 2/2 3/3
f 1/1 2/2 4/4 5/5"""


{-| Two triangles, s off, with UV coordinates, no explicit normals.
Each triangle gets 3 unshared vertices (flat shading).
-}
sOffTexturedObj : String
sOffTexturedObj =
    """v 0 0 0
v 1 0 0
v 1 1 0
v 0 1 0
vt 0 0
vt 1 0
vt 1 1
vt 0 1
s off
f 1/1 2/2 3/3
f 1/1 3/3 4/4"""


{-| Two co-planar triangles sharing an edge, s 1, with UV coordinates.
Positions 2 and 4 share the same UV in both faces so they are deduplicated:
4 vertices total.
-}
s1SmoothTexturedObj : String
s1SmoothTexturedObj =
    """v 0 0 0
v 1 0 0
v 2 0 0
v 1 1 0
vt 0 0
vt 0.5 0
vt 1 0
vt 0.5 1
s 1
f 1/1 2/2 4/4
f 2/2 3/3 4/4"""


{-| Two triangles with different smoothing groups sharing positions 1 and 2,
with UV coordinates. Vertex splitting produces 6 vertices total.
-}
mixedSGroupsTexturedObj : String
mixedSGroupsTexturedObj =
    """v 0 0 0
v 1 0 0
v 0 1 0
v 0 0 1
vt 0 0
vt 1 0
vt 0 1
vt 0 0
s 1
f 1/1 2/2 3/3
s 2
f 1/4 2/2 4/3"""


{-| Single triangle on XY plane, s off, no explicit normals.
Triangle (0,0,0),(1,0,0),(0,1,0) is CCW when viewed from +Z.
-}
xyPlaneObj : String
xyPlaneObj =
    """v 0 0 0
v 1 0 0
v 0 1 0
s off
f 1 2 3"""


{-| Single triangle on XY plane, s 1, no explicit normals.
Triangle (0,0,0),(1,0,0),(0,1,0) is CCW when viewed from +Z.
-}
xyPlaneSmoothObj : String
xyPlaneSmoothObj =
    """v 0 0 0
v 1 0 0
v 0 1 0
s 1
f 1 2 3"""


{-| Single triangle on XY plane, s off, with UV coordinates, no explicit normals.
Triangle (0,0,0),(1,0,0),(0,1,0) is CCW when viewed from +Z.
-}
xyPlaneTexturedObj : String
xyPlaneTexturedObj =
    """v 0 0 0
v 1 0 0
v 0 1 0
vt 0 0
vt 1 0
vt 0 1
s off
f 1/1 2/2 3/3"""


{-| Single triangle on XY plane, s 1, with UV coordinates, no explicit normals.
Triangle (0,0,0),(1,0,0),(0,1,0) is CCW when viewed from +Z.
-}
xyPlaneSmoothTexturedObj : String
xyPlaneSmoothTexturedObj =
    """v 0 0 0
v 1 0 0
v 0 1 0
vt 0 0
vt 1 0
vt 0 1
s 1
f 1/1 2/2 3/3"""


{-| Single quad on XY plane, s off, with UV coordinates, no explicit normals.
Quad (0,0,0),(1,0,0),(1,1,0),(0,1,0) is CCW when viewed from +Z.
Fan triangulation produces two triangles; both must have +Z normals.
-}
xyPlaneQuadTexturedObj : String
xyPlaneQuadTexturedObj =
    """v 0 0 0
v 1 0 0
v 1 1 0
v 0 1 0
vt 0 0
vt 1 0
vt 1 1
vt 0 1
s off
f 1/1 2/2 3/3 4/4"""
