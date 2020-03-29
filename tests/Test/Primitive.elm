module Test.Primitive exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Vm.Error as Error exposing (Error)
import Vm.Primitive as P
import Vm.Type as T


primitives : Test
primitives =
    describe "Primitives that return a value"
        [ describe "first"
            [ fuzz string "works with a string" <|
                \string ->
                    let
                        result =
                            string |> T.Word |> P.first
                    in
                        if String.length string > 0 then
                            result
                                |> Expect.equal (Ok <| T.Word <| String.left 1 string)
                        else
                            result
                                |> Expect.err
            , fuzz (list string) "works with a list of strings" <|
                \list ->
                    let
                        result =
                            list |> List.map T.Word |> T.List |> P.first
                    in
                        case list of
                            first :: rest ->
                                result
                                    |> Expect.equal (Ok <| T.Word <| first)

                            _ ->
                                result
                                    |> Expect.err
            ]
        , describe "butfirst"
            [ fuzz string "works with a string" <|
                \string ->
                    let
                        result =
                            string |> T.Word |> P.butfirst
                    in
                        if String.length string == 0 then
                            result
                                |> Expect.err
                        else
                            result
                                |> Expect.equal (Ok <| T.Word <| String.dropLeft 1 string)
            , fuzz (list string) "works with a list of strings" <|
                \list ->
                    let
                        result =
                            list |> List.map T.Word |> T.List |> P.butfirst
                    in
                        case list of
                            first :: rest ->
                                result
                                    |> Expect.equal (Ok <| T.List <| List.map T.Word <| rest)

                            _ ->
                                result
                                    |> Expect.err
            ]
        , describe "count"
            [ fuzz string "works with a string" <|
                \string ->
                    let
                        length =
                            String.length string
                    in
                        Expect.equal (P.count <| T.Word <| string) (Ok <| T.Word <| toString length)
            ]
        , describe "equalp"
            [ test "works with numbers" <|
                \_ -> Expect.equal (Ok <| T.Word "true") (P.equalp (T.Int 5) (T.Int 5))
            , test "works with lists" <|
                \_ ->
                    let
                        list =
                            T.List [ T.List [], T.Word "foo" ]
                    in
                        Expect.equal (Ok <| T.Word "true") (P.equalp list list)
            , test "returns false" <|
                \_ -> Expect.equal (Ok <| T.false) (P.equalp (T.Int 5) (T.Int 3))
            ]
        , describe "sum"
            [ test "works with number" <|
                \_ -> Expect.equal (Ok <| T.Int 23) (P.sum (T.Int 20) (T.Int 3))
            , test "works with words" <|
                \_ -> Expect.equal (Ok <| T.Float 23) (P.sum (T.Word "20") (T.Word "3"))
            ]
        , describe "difference"
            [ test "works with number" <|
                \_ -> Expect.equal (Ok <| T.Int 17) (P.difference (T.Int 20) (T.Int 3))
            , test "works with words" <|
                \_ -> Expect.equal (Ok <| T.Float 17) (P.difference (T.Word "20") (T.Word "3"))
            ]
        , describe "product"
            [ test "works with number" <|
                \_ -> Expect.equal (Ok <| T.Int 60) (P.product (T.Int 20) (T.Int 3))
            , test "works with words" <|
                \_ -> Expect.equal (Ok <| T.Float 60) (P.product (T.Word "20") (T.Word "3"))
            ]
        , describe "quotient"
            [ test "works with number" <|
                \_ -> Expect.equal (Ok <| T.Int 5) (P.quotient (T.Int 20) (T.Int 4))
            , test "works with words" <|
                \_ -> Expect.equal (Ok <| T.Float 5) (P.quotient (T.Word "20") (T.Word "4"))
            , test "errors if division by zero (numbers)" <|
                \_ -> Expect.equal (Err <| Error.WrongInput "quotient" "0") (P.quotient (T.Int 20) (T.Int 0))
            , test "errors if division by zero (words)" <|
                \_ -> Expect.equal (Err <| Error.WrongInput "quotient" "0") (P.quotient (T.Word "20") (T.Word "0"))
            ]
        , describe "minus"
            [ test "works with number" <|
                \_ -> Expect.equal (Ok <| T.Float -20) (P.minus (T.Int 20))
            , test "works with words" <|
                \_ -> Expect.equal (Ok <| T.Float -20) (P.minus (T.Word "20"))
            ]
        , describe "integerp"
            [ test "works with an integer" <|
                \_ ->
                    Expect.equal (P.integerp <| T.Int <| 10) (Ok <| T.true)
            , test "works with a string" <|
                \_ ->
                    Expect.equal (P.integerp <| T.Word <| "word") (Ok <| T.false)
            ]
        , describe "boolp"
            [ test "works with a string" <|
                \_ -> Expect.equal (P.boolp <| T.Word <| "word") (Ok <| T.false)
            , test "works with \"true" <|
                \_ -> Expect.equal (P.boolp <| T.Word <| "true") (Ok <| T.true)
            , test "works with an integer" <|
                \_ -> Expect.equal (P.boolp <| T.Int <| 10) (Ok <| T.false)
            ]
        , describe "remainder"
            [ test "works if result is 0" <|
                \_ -> Expect.equal (P.remainder (T.Word "20") (T.Word "5")) (Ok <| T.Int 0)
            , test "works if result is not 0" <|
                \_ -> Expect.equal (P.remainder (T.Word "20") (T.Word "3")) (Ok <| T.Int 2)
            ]
        ]
