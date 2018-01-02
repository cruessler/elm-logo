module Command exposing (..)

import Array
import Environment exposing (Environment)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Vm.Command as C
import Vm.Type exposing (Value(..))


commands : Test
commands =
    describe "Commands that output values"
        [ describe "print"
            [ fuzz string "prints strings" <|
                \string ->
                    let
                        result =
                            Environment.empty
                                |> C.print (Word string)
                    in
                        Expect.equal result
                            (Ok
                                { lines = Array.fromList [ string ]
                                }
                            )
            ]
        ]
