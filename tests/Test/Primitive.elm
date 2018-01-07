module Test.Primitive exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
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
        ]
