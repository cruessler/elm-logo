module Test.Command exposing (commands)

import Environment exposing (Environment)
import Environment.History exposing (Entry(..))
import Expect
import Fuzz exposing (string)
import Test exposing (Test, describe, fuzz)
import Vm.Command as C
import Vm.Type as Type


empty : Environment
empty =
    Environment.empty


commands : Test
commands =
    describe "Commands that output values"
        [ describe "print"
            [ fuzz string "prints strings" <|
                \string ->
                    let
                        result =
                            empty
                                |> C.print (Type.Word string)

                        lines =
                            String.lines string

                        entries =
                            lines
                                |> List.indexedMap (\i line -> ( i, Output line ))
                                |> List.reverse

                        expectedResult =
                            Ok
                                { empty
                                    | history = entries
                                    , nextId = List.length entries
                                }
                    in
                    Expect.equal result expectedResult
            ]
        ]
