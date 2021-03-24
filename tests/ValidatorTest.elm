module ValidatorTest exposing (..)

import Expect
import Test exposing (Test, describe, test)
import Validator


suite : Test
suite =
    describe "Validation test"
        [ test "validate happy hello!" <|
            \_ ->
                let
                    happy =
                        Validator.succeed identity
                            |> Validator.required identity String.isEmpty "No hello!" (Validator.custom Ok)
                in
                Expect.equal (Validator.run happy "hello!") (Ok "hello!")
        , test "Unhappy message" <|
            \_ ->
                let
                    unhappy =
                        Validator.succeed (\x -> { hello = x })
                            |> Validator.required .hello String.isEmpty "No hello!" (Validator.custom Ok)
                in
                Expect.equal (Validator.run unhappy { hello = "" }) (Err [ "No hello!" ])
        , test "Unhappy record" <|
            \_ ->
                let
                    unhappy =
                        Validator.succeed (\x y -> { a = x, b = y })
                            |> Validator.required .a String.isEmpty "a is required" (Validator.custom Ok)
                            |> Validator.required .b String.isEmpty "b is required" (Validator.custom Ok)
                in
                Expect.equal (Validator.run unhappy { a = "", b = "" }) (Err [ "a is required", "b is required" ])
        , test "Mixed record" <|
            \_ ->
                let
                    mixed =
                        Validator.succeed (\x y -> { a = x, b = y })
                            |> Validator.required .a String.isEmpty "a is required" (Validator.custom Ok)
                            |> Validator.required .b String.isEmpty "b is required" (Validator.custom Ok)
                in
                Expect.equal (Validator.run mixed { a = "hello!", b = "" }) (Err [ "b is required" ])
        , test "Optional happy saylor" <|
            \_ ->
                let
                    optionaHappy =
                        Validator.succeed (\x y -> { a = x, b = y })
                            |> Validator.required .a String.isEmpty "a is required" (Validator.custom Ok)
                            |> Validator.optional .b String.isEmpty "saylor" (Validator.custom Ok)
                in
                Expect.equal (Validator.run optionaHappy { a = "hello", b = "" }) (Ok { a = "hello", b = "saylor" })
        , test "Optional unhappy saylor" <|
            \_ ->
                let
                    optionaUnhappy =
                        Validator.succeed (\x y -> { a = x, b = y })
                            |> Validator.required .a String.isEmpty "a is required" (Validator.custom Ok)
                            |> Validator.optional .b String.isEmpty 0 (Validator.custom (String.toInt >> Result.fromMaybe [ "b is not a number" ]))
                in
                Expect.equal (Validator.run optionaUnhappy { a = "hello", b = "saylor" }) (Err [ "b is not a number" ])
        ]
