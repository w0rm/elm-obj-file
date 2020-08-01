module AdvancedDecoding exposing (andThen, combine, fail, oneOf, succeed)

import Expect
import Length
import Obj.Decode as Decode
import Test exposing (Test)


oneOf : Test
oneOf =
    Test.describe "oneOf"
        [ Test.test "lists error messages from all failed decoders" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters
                        (Decode.oneOf
                            [ Decode.object "missing-object" Decode.texturedFaces
                            , Decode.fail "Custom error"
                            ]
                        )
                    |> Expect.equal (Err "Failed oneOf decoder: No faces found for object 'missing-object', Custom error.")
        , Test.test "fails when no decoders were provided"
            (\_ ->
                objFile
                    |> Decode.decodeString Length.centimeters (Decode.oneOf [])
                    |> Expect.equal (Err "Empty oneOf decoder")
            )
        , Test.test "succeedes with the first succesful decoder" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters
                        (Decode.oneOf
                            [ Decode.fail "error"
                            , Decode.succeed "success 1"
                            , Decode.succeed "success 2"
                            ]
                        )
                    |> Expect.equal (Ok "success 1")
        ]


fail : Test
fail =
    Test.describe "fail"
        [ Test.test "fails with the given error message" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters
                        (Decode.fail "Unexpected")
                    |> Expect.equal (Err "Unexpected")
        ]


succeed : Test
succeed =
    Test.describe "succeed"
        [ Test.test "succeeds with the given result" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters
                        (Decode.succeed "Success")
                    |> Expect.equal (Ok "Success")
        ]


andThen : Test
andThen =
    Test.describe "andThen"
        [ Test.test "works for the successful case" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters
                        (Decode.succeed "Success"
                            |> Decode.andThen (\str -> Decode.succeed (str ++ " Success"))
                        )
                    |> Expect.equal (Ok "Success Success")
        ]


combine : Test
combine =
    Test.describe "combine"
        [ Test.test "works for the successful case" <|
            \_ ->
                objFile
                    |> Decode.decodeString Length.centimeters
                        (Decode.combine
                            [ Decode.succeed "One"
                            , Decode.succeed "Two"
                            , Decode.succeed "Three"
                            ]
                        )
                    |> Expect.equal (Ok [ "One", "Two", "Three" ])
        ]


objFile : String
objFile =
    """o object1
v -0.126193 0.126193 -0.126193
v -0.126193 0.126193 0.126193
v -0.126193 -0.126193 -0.126193
vt 0.106151 0.902035
vt 0.128224 0.902035
vt 0.128224 0.913071
vn 0.0000 -1.0000 0.0000
vn -1.0000 0.0000 0.0000
vn 0.0000 0.0000 -1.0000
usemtl generic
f 1/1/1 2/2/2 3/3/3
"""
