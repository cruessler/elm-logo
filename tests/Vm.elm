module Vm exposing (..)

import Array
import Environment
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Vm.Command as C
import Vm.Introspect as I
import Vm.Primitive as P
import Vm.Scope as Scope
import Vm.Type as Type
import Vm.Vm exposing (..)


emptyVm : Vm
emptyVm =
    { instructions = Array.empty
    , programCounter = 0
    , stack = []
    , scopes = Scope.empty
    , environment = Environment.empty
    }


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
                    [ PushValue <| Type.Word "word"
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
                    Expect.equal vm.stack [ Type.Word "w" ]
            ]


vmWithVariables : Test
vmWithVariables =
    let
        vm =
            { emptyVm
                | instructions =
                    [ PushValue <| Type.Word "word"
                    , StoreVariable "a"
                    , PushVariable "a"
                    ]
                        |> Array.fromList
            }
                |> runAndUnwrap
    in
        test "setting and getting a variable" <|
            \_ ->
                Expect.equal vm.stack [ Type.Word "word" ]


vmWithIntrospection : Test
vmWithIntrospection =
    let
        vm =
            { emptyVm
                | instructions =
                    [ Introspect0 { name = "repcount", f = I.repcount } ]
                        |> Array.fromList
            }
                |> runAndUnwrap
    in
        test "calling repcount outside a loop" <|
            \_ ->
                Expect.equal vm.stack [ Type.Word "-1" ]


vmWithConditionalPrint : Test
vmWithConditionalPrint =
    let
        vm =
            { emptyVm
                | instructions =
                    [ PushValue <| Type.Word "true"
                    , JumpIfFalse 4
                    , PushValue <| Type.Word "first"
                    , Command1 { name = "print", f = C.print }
                    , Jump 3
                    , PushValue <| Type.Word "second"
                    , Command1 { name = "print", f = C.print }
                    ]
                        |> Array.fromList
            }
                |> runAndUnwrap
    in
        describe "with conditional print" <|
            [ test "program counter gets incremented" <|
                \_ ->
                    Expect.equal vm.programCounter 7
            , test "stack is empty" <|
                \_ ->
                    Expect.equal vm.stack []
            , test "environment contains printed line" <|
                \_ ->
                    Expect.equal vm.environment.lines <| Array.fromList [ "first" ]
            ]


vmWithPrintLoop : Test
vmWithPrintLoop =
    let
        vm =
            { emptyVm
                | instructions =
                    [ PushLoopScope
                    , PushValue (Type.Word "10")
                    , Introspect0 { name = "repcount", f = I.repcount }
                    , Eval2 { name = "lessThan", f = P.lessThan }
                    , JumpIfFalse 5
                    , EnterLoopScope
                    , PushValue (Type.Word "word")
                    , Command1 { name = "print", f = C.print }
                    , Jump -7
                    , PopLoopScope
                    ]
                        |> Array.fromList
            }
                |> runAndUnwrap
    in
        describe "with print loop" <|
            [ test "program counter gets incremented" <|
                \_ -> Expect.equal vm.programCounter 10
            , test "stack is empty" <|
                \_ -> Expect.equal vm.stack []
            , test "environment contains printed lines" <|
                \_ -> Expect.equal vm.environment.lines (Array.repeat 10 "word")
            ]


vmWithTemplateLoop : Test
vmWithTemplateLoop =
    let
        vm =
            { emptyVm
                | instructions =
                    [ PushValue (Type.Word "word")
                    , PushTemplateScope
                    , PushValue (Type.Word "rest")
                    , Introspect1 { name = "?", f = I.templateVariable }
                    , Eval1 { name = "emptyp", f = P.emptyp }
                    , JumpIfTrue 6
                    , EnterTemplateScope
                    , PushValue (Type.Word "1")
                    , Introspect1 { name = "?", f = I.templateVariable }
                    , Command1 { name = "print", f = C.print }
                    , Jump -8
                    , PopTemplateScope
                    ]
                        |> Array.fromList
            }
                |> runAndUnwrap
    in
        describe "with print in a template loop" <|
            [ test "environment contains printed lines" <|
                \_ -> Expect.equal vm.environment.lines (Array.fromList [ "w", "o", "r", "d" ])
            ]


vmWithLocalScope : Test
vmWithLocalScope =
    let
        vm =
            { emptyVm
                | instructions =
                    [ PushValue (Type.Int 0)
                    , PushLocalScope
                    , LocalVariable "arg"
                    , PushValue (Type.Word "value")
                    , StoreVariable "arg"
                    , PushVariable "arg"
                    , Command1 { name = "print", f = C.print }
                    , PopLocalScope
                    ]
                        |> Array.fromList
            }
                |> runAndUnwrap
    in
        describe "with print in a local scope" <|
            [ test "environment contains printed lines" <|
                \_ -> Expect.equal vm.environment.lines (Array.fromList [ "value" ])
            ]
