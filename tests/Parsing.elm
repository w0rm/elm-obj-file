module Parsing exposing (parsing)

import Expect
import Length
import Obj.Decode as Decode
import Test exposing (Test)


parsing : Test
parsing =
    Test.describe "parsing"
        [ Test.test "passes for an empty string" <|
            \_ ->
                ""
                    |> Decode.decodeString Length.centimeters (Decode.succeed "success")
                    |> Expect.equal (Ok "success")
        , Test.test "passes for valid format" <|
            \_ ->
                objFile defaults
                    |> Decode.decodeString Length.centimeters (Decode.succeed "success")
                    |> Expect.equal (Ok "success")
        , Test.test "fails for a missing object name" <|
            \_ ->
                objFile { defaults | objectName = "" }
                    |> Decode.decodeString Length.centimeters (Decode.succeed "success")
                    |> Expect.equal (Err "Line 1: No object name")
        , Test.test "fails for missing group names" <|
            \_ ->
                objFile { defaults | groupNames = "" }
                    |> Decode.decodeString Length.centimeters (Decode.succeed "success")
                    |> Expect.equal (Err "Line 11: No groups specified")
        , Test.test "fails for a missing material name" <|
            \_ ->
                objFile { defaults | materialName = "" }
                    |> Decode.decodeString Length.centimeters (Decode.succeed "success")
                    |> Expect.equal (Err "Line 12: No material name")
        , Test.test "fails for invalid position format" <|
            \_ ->
                objFile { defaults | position = "-0.126193 0.126193" }
                    |> Decode.decodeString Length.centimeters (Decode.succeed "success")
                    |> Expect.equal (Err "Line 3: Invalid position format")
        , Test.test "fails for invalid texture coordinates format" <|
            \_ ->
                objFile { defaults | textureCoordinates = "-0.126193" }
                    |> Decode.decodeString Length.centimeters (Decode.succeed "success")
                    |> Expect.equal (Err "Line 6: Invalid texture coordinates format")
        , Test.test "fails for invalid normal vector format" <|
            \_ ->
                objFile { defaults | normal = "-0.126193" }
                    |> Decode.decodeString Length.centimeters (Decode.succeed "success")
                    |> Expect.equal (Err "Line 9: Invalid normal vector format")
        , Test.test "fails for invalid face format" <|
            \_ ->
                objFile { defaults | faceIndices = "invalid" }
                    |> Decode.decodeString Length.centimeters (Decode.succeed "success")
                    |> Expect.equal (Err "Line 14: Invalid face format")
        , Test.test "fails for a face with no vertices" <|
            \_ ->
                objFile { defaults | faceIndices = "" }
                    |> Decode.decodeString Length.centimeters (Decode.succeed "success")
                    |> Expect.equal (Err "Line 14: Face has no vertices")
        , Test.test "fails for a face with less than three vertices" <|
            \_ ->
                objFile { defaults | faceIndices = "1/1/1 2/2/2" }
                    |> Decode.decodeString Length.centimeters (Decode.succeed "success")
                    |> Expect.equal (Err "Line 14: Face has less than three vertices")
        , Test.test "fails for invalid line format" <|
            \_ ->
                objFile { defaults | lineIndices = "invalid" }
                    |> Decode.decodeString Length.centimeters (Decode.succeed "success")
                    |> Expect.equal (Err "Line 15: Invalid line format")
        , Test.test "fails for a line with no vertices" <|
            \_ ->
                objFile { defaults | lineIndices = "" }
                    |> Decode.decodeString Length.centimeters (Decode.succeed "success")
                    |> Expect.equal (Err "Line 15: Line has no vertices")
        , Test.test "fails for a line with less than two points" <|
            \_ ->
                objFile { defaults | lineIndices = "1" }
                    |> Decode.decodeString Length.centimeters (Decode.succeed "success")
                    |> Expect.equal (Err "Line 15: Line has less than two vertices")
        , Test.test "fails for invalid points format" <|
            \_ ->
                objFile { defaults | pointsIndices = "invalid" }
                    |> Decode.decodeString Length.centimeters (Decode.succeed "success")
                    |> Expect.equal (Err "Line 16: Invalid points format")
        , Test.test "fails for points with no vertices" <|
            \_ ->
                objFile { defaults | pointsIndices = "" }
                    |> Decode.decodeString Length.centimeters (Decode.succeed "success")
                    |> Expect.equal (Err "Line 16: Points element has no vertices")
        ]


type alias Settings =
    { objectName : String
    , groupNames : String
    , materialName : String
    , position : String
    , textureCoordinates : String
    , normal : String
    , faceIndices : String
    , lineIndices : String
    , pointsIndices : String
    }


defaults : Settings
defaults =
    { objectName = "Cube"
    , groupNames = "Test"
    , materialName = "Material"
    , position = "-0.126193 0.126193 0.126193"
    , textureCoordinates = "0.128224 0.902035"
    , normal = "-1.0000 0.0000 0.0000"
    , faceIndices = "1/1/1 2/2/2 3/3/3"
    , lineIndices = "1/1 2/2 3/3"
    , pointsIndices = "1 2 3"
    }


objFile : Settings -> String
objFile { objectName, groupNames, materialName, position, textureCoordinates, normal, faceIndices, lineIndices, pointsIndices } =
    String.join "\n"
        [ "o " ++ objectName
        , "v -0.126193 0.126193 -0.126193"
        , "v " ++ position
        , "v -0.126193 -0.126193 -0.126193"
        , "vt 0.106151 0.902035"
        , "vt " ++ textureCoordinates
        , "vt 0.128224 0.913071"
        , "vn 0.0000 -1.0000 0.0000"
        , "vn " ++ normal
        , "vn 0.0000 0.0000 -1.0000"
        , "g " ++ groupNames
        , "usemtl " ++ materialName
        , "s off"
        , "f " ++ faceIndices
        , "l " ++ lineIndices
        , "p " ++ pointsIndices
        ]
