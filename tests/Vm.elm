module Vm exposing (..)

import Array
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Vm.Primitives as P
import Vm.Types as T
import Vm.Vm exposing (..)


emptyVm : Vm
emptyVm =
    { instructions = Array.empty, programCounter = 0, stack = [] }


{-| Run a `Vm`. In case an error occurs, it is swallowed and an empty `Vm` is
returned instead.

When using this function, tests don’t have to match on the whole state of the
`Vm` (as in `Expect.equal vm Ok { vm | programCounter = 1}`), but can compare
its individual pieces to expected values (as in `Expect.equal vm.programCounter
1`). This way, test can be written with a higher granularity.

-}
runAndUnwrap : Vm -> Vm
runAndUnwrap vm =
    vm
        |> run
        |> Result.withDefault emptyVm


vmWithTwoInstructions : Test
vmWithTwoInstructions =
    let
        vm =
            { emptyVm
                | instructions =
                    [ PutValue <| T.Word "word"
                    , Eval1 { name = "first", f = P.first }
                    ]
                        |> Array.fromList
            }
                |> runAndUnwrap
    in
        describe "with two instructions" <|
            [ test "program counter gets incremented" <|
                \_ ->
                    Expect.equal vm.programCounter 2
            , test "stack gets changed" <|
                \_ ->
                    Expect.equal vm.stack [ T.Word "w" ]
            ]
