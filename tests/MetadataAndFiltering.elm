module MetadataAndFiltering exposing (defaultGroup, filter, group, groupNames, material, materialNames, object, objectNames)

import Array
import Expect
import Length
import Obj.Decode as Decode
import Test exposing (Test)
import TriangularMesh


objectNames : Test
objectNames =
    Test.describe "objectNames"
        [ Test.test "returns a list of object names from the file" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters Decode.objectNames
                    |> Expect.equal (Ok [ "Cube", "Lines" ])
        , Test.test "returns an empty list when no objects were found in a file" <|
            \_ ->
                ""
                    |> Decode.decodeString Length.centimeters Decode.objectNames
                    |> Expect.equal (Ok [])
        ]


materialNames : Test
materialNames =
    Test.describe "materialNames"
        [ Test.test "returns a list of material names from the file" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters Decode.materialNames
                    |> Expect.equal (Ok [ "Material1", "Material2" ])
        , Test.test "returns an empty list when no materials were found in a file" <|
            \_ ->
                ""
                    |> Decode.decodeString Length.centimeters Decode.materialNames
                    |> Expect.equal (Ok [])
        ]


groupNames : Test
groupNames =
    Test.describe "groupNames"
        [ Test.test "returns a list of group names from the file" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters Decode.groupNames
                    |> Expect.equal (Ok [ "Face2", "Face3", "Face4", "Face5", "Face6", "Faces", "default" ])
        , Test.test "returns an empty list when no groups were found in a file" <|
            \_ ->
                ""
                    |> Decode.decodeString Length.centimeters Decode.groupNames
                    |> Expect.equal (Ok [])
        ]


object : Test
object =
    Test.describe "object"
        [ Test.test "decodes material names for a certain object" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters (Decode.object "Cube" Decode.materialNames)
                    |> Expect.equal (Ok [ "Material1" ])
        , Test.test "the not found error contains object name" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters (Decode.object "Lines" Decode.triangles)
                    |> Expect.equal (Err "No faces found for object 'Lines'")
        ]


material : Test
material =
    Test.describe "material"
        [ Test.test "decodes object names for a certain material" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters (Decode.material "Material2" Decode.objectNames)
                    |> Expect.equal (Ok [ "Lines" ])
        , Test.test "the not found error contains the material name" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters (Decode.material "Material1" Decode.polylines)
                    |> Expect.equal (Err "No lines found for material 'Material1'")
        ]


defaultGroup : Test
defaultGroup =
    Test.describe "defaultGroup"
        [ Test.test "decodes triangles in the default group" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters (Decode.defaultGroup Decode.triangles)
                    |> Result.map
                        (\triangularMesh ->
                            ( Array.length (TriangularMesh.vertices triangularMesh)
                            , List.length (TriangularMesh.faceIndices triangularMesh)
                            )
                        )
                    -- the default group has 4 vertices and 2 triangles
                    |> Expect.equal (Ok ( 4, 2 ))
        , Test.test "the not found error contains the default group name" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters (Decode.defaultGroup Decode.polylines)
                    |> Expect.equal (Err "No lines found for group 'default'")
        ]


group : Test
group =
    Test.describe "group"
        [ Test.test "decodes triangles in a group" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters (Decode.group "Faces" Decode.triangles)
                    |> Result.map
                        (\triangularMesh ->
                            ( Array.length (TriangularMesh.vertices triangularMesh)
                            , List.length (TriangularMesh.faceIndices triangularMesh)
                            )
                        )
                    |> Expect.equal (Ok ( 8, 10 ))
        , Test.test "the not found error contains the group name" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters (Decode.group "Bla" Decode.faces)
                    |> Expect.equal (Err "No faces found for group 'Bla'")
        ]


filter : Test
filter =
    Test.describe "filter"
        [ Test.test "decodes triangles in selected groups" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters
                        (Decode.filter
                            (\{ groups } -> List.member "Face2" groups || List.member "Face3" groups)
                            Decode.triangles
                        )
                    |> Result.map
                        (\triangularMesh ->
                            ( Array.length (TriangularMesh.vertices triangularMesh)
                            , List.length (TriangularMesh.faceIndices triangularMesh)
                            )
                        )
                    |> Expect.equal (Ok ( 6, 4 ))
        , Test.test "the not found error contains <custom filter>" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters (Decode.filter (\_ -> False) Decode.faces)
                    |> Expect.equal (Err "No faces found for <custom filter>")
        ]


objFile : String
objFile =
    """# Blender v2.80 (sub 75) OBJ File: ''
# www.blender.org
mtllib untitled.mtl
o Cube
v 1.000000 1.000000 -1.000000
v 1.000000 -1.000000 -1.000000
v 1.000000 1.000000 1.000000
v 1.000000 -1.000000 1.000000
v -1.000000 1.000000 -1.000000
v -1.000000 -1.000000 -1.000000
v -1.000000 1.000000 1.000000
v -1.000000 -1.000000 1.000000
v -1.260743 1.051649 1.526675
v -1.090024 1.399034 0.711974
v -0.949402 2.107505 -0.479652
vt 0.375000 0.000000
vt 0.625000 0.000000
vt 0.625000 0.250000
vt 0.375000 0.250000
vt 0.375000 0.250000
vt 0.625000 0.250000
vt 0.625000 0.500000
vt 0.375000 0.500000
vt 0.625000 0.750000
vt 0.375000 0.750000
vt 0.625000 0.750000
vt 0.625000 1.000000
vt 0.375000 1.000000
vt 0.125000 0.500000
vt 0.375000 0.500000
vt 0.375000 0.750000
vt 0.125000 0.750000
vt 0.625000 0.500000
vt 0.875000 0.500000
vt 0.875000 0.750000
vn 0.0000 1.0000 0.0000
vn 0.0000 0.0000 1.0000
vn -1.0000 0.0000 0.0000
vn 0.0000 -1.0000 0.0000
vn 1.0000 0.0000 0.0000
vn 0.0000 0.0000 -1.0000
usemtl Material1
f 1/1/1 5/2/1 7/3/1 3/4/1
g Faces Face2
f 4/5/2 3/6/2 7/7/2 8/8/2
g Faces Face3
f 8/8/3 7/7/3 5/9/3 6/10/3
g Faces Face4
f 6/10/4 2/11/4 4/12/4 8/13/4
g Faces Face5
f 2/14/5 1/15/5 3/16/5 4/17/5
g Faces Face6
f 6/18/6 5/19/6 1/20/6 2/11/6
o Lines
usemtl Material2
l 9 10
l 1 2 3"""
