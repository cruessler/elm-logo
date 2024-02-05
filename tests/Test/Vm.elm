module Test.Vm exposing (..)

import Array
import Dict
import Environment
import Environment.History exposing (Entry(..), History)
import Expect
import Test exposing (..)
import Vm.Command as C
import Vm.Exception as Exception
import Vm.Instruction exposing (Instruction(..))
import Vm.Introspect as I
import Vm.Primitive as P
import Vm.Scope as Scope
import Vm.Stack as Stack
import Vm.Type as Type
import Vm.Vm exposing (State(..), Vm, run)


emptyVm : Vm
emptyVm =
    { instructions = Array.empty
    , programCounter = 0
    , stack = []
    , scopes = Scope.empty
    , environment = Environment.empty
    , functionTable = Dict.empty
    , compiledFunctions = []
    }


{-| Run a `Vm`.

When using this function, tests don’t have to match on the whole state of the
`Vm` (as in `Expect.equal vm Done { vm | programCounter = 1}`), but can
compare its individual pieces to expected values (as in `Expect.equal
vm.programCounter 1`). This way, test can be written with a higher granularity.

-}
runAndUnwrap : Vm -> Vm
runAndUnwrap vm =
    case run vm of
        Done vm_ ->
            vm_

        Paused vm_ ->
            vm_


outputLines : List String -> History
outputLines =
    List.indexedMap (\i line -> ( i, Output line ))


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
                Expect.equal vm.stack [ Stack.Value <| Stack.Word "w" ]
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
            Expect.equal vm.stack [ Stack.Value <| Stack.Word "word" ]


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
            Expect.equal vm.stack [ Stack.Value <| Stack.Int -1 ]


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
                Expect.equal vm.environment.history <| [ ( 0, Output "first" ) ]
        ]


vmWithPrintLoop : Test
vmWithPrintLoop =
    let
        vm =
            { emptyVm
                | instructions =
                    [ PushValue (Type.Word "10")
                    , Duplicate
                    , Eval1 { name = "integerp", f = P.integerp }
                    , JumpIfTrue 2
                    , Raise (Exception.WrongInput "repeat")
                    , PushLoopScope
                    , EnterLoopScope
                    , JumpIfTrue 4
                    , PushValue (Type.Word "word")
                    , Command1 { name = "print", f = C.print }
                    , Jump -4
                    , PopLoopScope
                    ]
                        |> Array.fromList
            }
                |> runAndUnwrap
    in
    describe "with print loop" <|
        [ test "program counter gets incremented" <|
            \_ -> Expect.equal vm.programCounter 12
        , test "stack is empty" <|
            \_ -> Expect.equal vm.stack []
        , test "environment contains printed lines" <|
            \_ ->
                Expect.equal vm.environment.history
                    (List.repeat 10 "word" |> outputLines |> List.reverse)
        ]


vmWithTemplateLoop : Test
vmWithTemplateLoop =
    let
        vm =
            { emptyVm
                | instructions =
                    [ PushValue (Type.Word "word")
                    , PushTemplateScope
                    , EnterTemplateScope
                    , JumpIfTrue 5
                    , PushValue (Type.Word "1")
                    , Introspect1 { name = "?", f = I.templateVariable }
                    , Command1 { name = "print", f = C.print }
                    , Jump -5
                    , PopTemplateScope
                    ]
                        |> Array.fromList
            }
                |> runAndUnwrap
    in
    describe "with print in a template loop" <|
        [ test "environment contains printed lines" <|
            \_ ->
                Expect.equal vm.environment.history
                    (outputLines [ "w", "o", "r", "d" ] |> List.reverse)
        ]


vmWithLoopScope : Test
vmWithLoopScope =
    let
        vm =
            { emptyVm
                | instructions =
                    [ PushValue (Type.Int 2)
                    , PushLoopScope
                    , EnterLoopScope
                    , JumpIfTrue 2
                    , Jump -2
                    , PopLoopScope
                    ]
                        |> Array.fromList
            }
                |> runAndUnwrap
    in
    describe "with empty repeat loop" <|
        [ test "stack is empty after loop is finished" <|
            \_ -> Expect.equal vm.stack []
        , test "program counter is beyond the last instruction" <|
            \_ -> Expect.equal vm.programCounter 6
        ]


vmWithLocalScope : Test
vmWithLocalScope =
    let
        vm =
            { emptyVm
                | stack = [ Stack.Address 0 ]
                , instructions =
                    [ PushLocalScope
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
            \_ -> Expect.equal vm.environment.history [ ( 0, Output "value" ) ]
        ]


vmWithFlip : Test
vmWithFlip =
    let
        vm =
            { emptyVm
                | stack =
                    [ Stack.Value (Stack.Word "a")
                    , Stack.Value (Stack.Word "b")
                    ]
                , instructions =
                    [ Flip ]
                        |> Array.fromList
            }
                |> runAndUnwrap
    in
    describe "Flip" <|
        [ test "flips topmost stack values" <|
            \_ ->
                Expect.equal vm.stack
                    [ Stack.Value (Stack.Word "b")
                    , Stack.Value (Stack.Word "a")
                    ]
        ]


vmWithSampleProgram : Test
vmWithSampleProgram =
    {- This test is a manual translation of the sample program given at
       <https://people.eecs.berkeley.edu/~bh/logo-sample.html>.
       > Here is a short but complete program in Berkeley Logo:
       >
       >   to choices :menu [:sofar []]
       >     if emptyp :menu [print :sofar stop]
       >     foreach first :menu [(choices butfirst :menu sentence :sofar ?)]
       >   end
       >
       > And here's how you use it. You type
       >
       >   choices [[small medium large]
       >       [vanilla [ultra chocolate] lychee [rum raisin] ginger]
       >       [cone cup]]
    -}
    let
        vm =
            { emptyVm
                | programCounter = 28
                , instructions =
                    [ PushLocalScope
                    , LocalVariable "menu"
                    , StoreVariable "menu"
                    , LocalVariable "sofar"
                    , StoreVariable "sofar"

                    -- if emptyp :menu
                    , PushVariable "menu"
                    , Eval1 { name = "emptyp", f = P.emptyp }
                    , JumpIfFalse 5

                    -- print :sofar
                    , PushVariable "sofar"
                    , Command1 { name = "print", f = C.print }

                    -- stop
                    , PopLocalScope
                    , Return

                    -- first :menu
                    , PushVariable "menu"
                    , Eval1 { name = "first", f = P.first }

                    -- foreach [ ...
                    , PushTemplateScope
                    , EnterTemplateScope
                    , JumpIfTrue 9

                    -- sentence :sofar ?
                    , PushValue (Type.Word "1")
                    , Introspect1 { name = "?", f = I.templateVariable }
                    , PushVariable "sofar"
                    , EvalN { name = "sentence", f = P.sentence, numberOfDefaultArguments = 2 } 2

                    -- butfirst :menu
                    , PushVariable "menu"
                    , Eval1 { name = "butfirst", f = P.butfirst }

                    -- choices ...
                    , Call 0
                    , Jump -9
                    , PopTemplateScope

                    -- ... ]
                    , PopLocalScope
                    , Return
                    , PushValue (Type.List [])
                    , PushValue
                        (Type.List
                            [ Type.List
                                [ Type.Word "small"
                                , Type.Word "medium"
                                , Type.Word "large"
                                ]
                            , Type.List
                                [ Type.Word "vanilla"
                                , Type.List [ Type.Word "ultra", Type.Word "chocolate" ]
                                , Type.Word "lychee"
                                , Type.List [ Type.Word "rum", Type.Word "raisin" ]
                                , Type.Word "ginger"
                                ]
                            , Type.List
                                [ Type.Word "cone"
                                , Type.Word "cup"
                                ]
                            ]
                        )

                    -- choices ...
                    , Call 0
                    ]
                        |> Array.fromList
                , functionTable = Dict.singleton "choices" 0
            }
                |> runAndUnwrap
    in
    describe "with print and recursive function calls" <|
        [ test "program counter is beyond the last instruction" <|
            \_ ->
                Expect.equal vm.programCounter 31
        , test "environment contains printed lines" <|
            \_ ->
                Expect.equal vm.environment.history
                    (outputLines
                        [ "small vanilla cone"
                        , "small vanilla cup"
                        , "small ultra chocolate cone"
                        , "small ultra chocolate cup"
                        , "small lychee cone"
                        , "small lychee cup"
                        , "small rum raisin cone"
                        , "small rum raisin cup"
                        , "small ginger cone"
                        , "small ginger cup"
                        , "medium vanilla cone"
                        , "medium vanilla cup"
                        , "medium ultra chocolate cone"
                        , "medium ultra chocolate cup"
                        , "medium lychee cone"
                        , "medium lychee cup"
                        , "medium rum raisin cone"
                        , "medium rum raisin cup"
                        , "medium ginger cone"
                        , "medium ginger cup"
                        , "large vanilla cone"
                        , "large vanilla cup"
                        , "large ultra chocolate cone"
                        , "large ultra chocolate cup"
                        , "large lychee cone"
                        , "large lychee cup"
                        , "large rum raisin cone"
                        , "large rum raisin cup"
                        , "large ginger cone"
                        , "large ginger cup"
                        ]
                        |> List.reverse
                    )
        ]


vmWithEvalOnList : Test
vmWithEvalOnList =
    let
        vm =
            { emptyVm
                | instructions =
                    [ PushValue (Type.List [ Type.Word "print", Type.Word "\"word" ])
                    , Eval
                    ]
                        |> Array.fromList
            }
                |> runAndUnwrap
    in
    describe "with eval on list" <|
        [ test "environment contains printed line" <|
            \_ -> Expect.equal vm.environment.history [ ( 0, Output "word" ) ]
        ]


vmWithEvalOnWord : Test
vmWithEvalOnWord =
    let
        vm =
            { emptyVm
                | instructions =
                    [ PushValue (Type.Int 90)
                    , Command1 { name = "forward", f = C.forward }
                    , PushValue (Type.Word "home")
                    , Eval
                    , PushValue (Type.Int 90)
                    , Command1 { name = "back", f = C.back }
                    ]
                        |> Array.fromList
            }
                |> runAndUnwrap
    in
    describe "with eval on word" <|
        [ test "turtle is reset before moving back" <|
            -- `runAndUnwrap` swallows errors, so we can’t expect `y` to be `0`
            -- which it would be if the program failed.
            \_ -> Expect.equal vm.environment.turtle.y 90
        ]
