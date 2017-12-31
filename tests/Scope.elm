module Scope exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Vm.Scope as Scope
    exposing
        ( Scope
        , Binding(..)
        )
import Vm.Type exposing (Value(..))


scopes : Test
scopes =
    describe "Commands that get or set variables"
        [ describe "make"
            [ test "sets variable in root scope" <|
                \_ ->
                    let
                        scope =
                            Scope.empty
                                |> Scope.make "x" (Word "word")
                    in
                        scope
                            |> Scope.thing "x"
                            |> Expect.equal (Just <| Defined <| Word "word")
            ]
        ]
