module Vm.Vm exposing
    ( State(..)
    , Vm
    , empty
    , getParser
    , initialize
    , run
    , step
    , toValue
    , withCompiledFunctions
    , withEnvironment
    )

{-| This module provides types and data structures for representing a virtual
machine as well as functions for running it.
-}

import Array exposing (Array)
import Compiler.Ast as Ast exposing (CompiledFunction, CompiledProgram, Program)
import Compiler.Linker as Linker exposing (LinkedProgram)
import Compiler.Parser as Parser exposing (Parser)
import Dict exposing (Dict)
import Environment exposing (Environment)
import Json.Encode as E
import Parser.Advanced as Parser
import Vm.Command as C
import Vm.Error as Error exposing (Error(..), Internal(..))
import Vm.Exception as Exception exposing (Exception)
import Vm.Instruction exposing (Instruction(..))
import Vm.Introspect as I
import Vm.Primitive as P
import Vm.Scope as Scope exposing (Binding(..), Scope)
import Vm.Stack as Stack exposing (Stack)
import Vm.Type as Type


{-| Represent a stack based virtual machine.
-}
type alias Vm =
    { instructions : Array Instruction
    , programCounter : Int
    , stack : Stack
    , scopes : List Scope
    , environment : Environment
    , functionTable : Dict String Int
    , compiledFunctions : List CompiledFunction
    }


{-| Initialize an empty `Vm`.
-}
empty : Vm
empty =
    initialize
        { instructions = []
        , functionTable = Dict.empty
        , compiledFunctions = []
        , startAddress = 0
        }


{-| Initialize a `Vm` with a list of instructions and a program counter.
-}
initialize : LinkedProgram -> Vm
initialize { instructions, functionTable, compiledFunctions, startAddress } =
    { instructions = Array.fromList instructions
    , programCounter = startAddress
    , stack = []
    , scopes = Scope.empty
    , environment = Environment.empty
    , functionTable = functionTable
    , compiledFunctions = compiledFunctions
    }


withCompiledFunctions : List CompiledFunction -> Vm -> Vm
withCompiledFunctions compiledFunctions vm =
    { vm
        | compiledFunctions = compiledFunctions
    }


withEnvironment : Environment -> Vm -> Vm
withEnvironment environment vm =
    { vm
        | environment = environment
    }


getParser : Vm -> Parser Ast.Program
getParser =
    .compiledFunctions
        >> List.map (\function -> ( function.name, function ))
        >> Dict.fromList
        >> Parser.withExistingFunctions


encodeInstruction : Instruction -> E.Value
encodeInstruction instruction =
    let
        toString : String
        toString =
            case instruction of
                PushValue value ->
                    "PushValue " ++ Type.toDebugString value

                PushVariable name ->
                    "PushVariable " ++ name

                StoreVariable name ->
                    "StoreVariable " ++ name

                LocalVariable name ->
                    "LocalVariable " ++ name

                Introspect0 { name } ->
                    "Introspect0 " ++ name

                Introspect1 { name } ->
                    "Introspect1 " ++ name

                Eval ->
                    "Eval"

                Eval1 { name } ->
                    "Eval1 " ++ name

                Eval2 { name } ->
                    "Eval2 " ++ name

                Eval3 { name } ->
                    "Eval3 " ++ name

                EvalN { name } n ->
                    "EvalN [" ++ String.fromInt n ++ "] " ++ name

                Command0 { name } ->
                    "Command0 " ++ name

                Command1 { name } ->
                    "Command1 " ++ name

                Command2 { name } ->
                    "Command2 " ++ name

                CommandN { name } n ->
                    "CommandN [" ++ String.fromInt n ++ "] " ++ name

                PushLoopScope ->
                    "PushLoopScope"

                EnterLoopScope ->
                    "EnterLoopScope"

                PopLoopScope ->
                    "PopLoopScope"

                PushTemplateScope ->
                    "PushTemplateScope"

                EnterTemplateScope ->
                    "EnterTemplateScope"

                PopTemplateScope ->
                    "PopTemplateScope"

                PushLocalScope ->
                    "PushLocalScope"

                PopLocalScope ->
                    "PopLocalScope"

                JumpIfFalse distance ->
                    "JumpIfFalse " ++ String.fromInt distance

                JumpIfTrue distance ->
                    "JumpIfTrue " ++ String.fromInt distance

                Jump distance ->
                    "Jump " ++ String.fromInt distance

                Call address ->
                    "Call " ++ String.fromInt address

                CallByName name ->
                    "CallByName " ++ name

                PushVoid ->
                    "PushVoid"

                Return ->
                    "Return"

                CheckReturn ->
                    "CheckReturn"

                Duplicate ->
                    "Duplicate"

                Flip ->
                    "Flip"

                Raise _ ->
                    "Raise"
    in
    toString |> E.string


encodeInstructions : Array Instruction -> E.Value
encodeInstructions =
    E.array encodeInstruction


toValue : Vm -> E.Value
toValue { instructions, programCounter, stack, scopes, environment } =
    E.object
        [ ( "instructions", encodeInstructions instructions )
        , ( "programCounter", E.int programCounter )
        , ( "stack", Stack.toValue stack )
        , ( "scopes", E.list Scope.toValue scopes )
        , ( "environment", Environment.toValue environment )
        ]


{-| Increment the program counter of a `Vm`.
-}
incrementProgramCounter : Vm -> Vm
incrementProgramCounter vm =
    { vm | programCounter = vm.programCounter + 1 }


{-| Push a single value onto the stack.
-}
pushValue1 : Type.Value -> Vm -> Vm
pushValue1 value vm =
    { vm | stack = Stack.Value value :: vm.stack }


{-| Pop a single value from the stack. Return a value and the remaining stack.

Return `Err (Internal InvalidStack)` if there is no `Stack.Value` on the stack.

-}
popValue1 : Vm -> Result Error ( Type.Value, Vm )
popValue1 vm =
    case vm.stack of
        (Stack.Value first) :: rest ->
            Ok ( first, { vm | stack = rest } )

        _ ->
            Err <| Internal InvalidStack


{-| Pop two values from the stack. Return both values and the remaining stack.

Return `Err (Internal InvalidStack)` if the two topmost items are not a
`Stack.Value`.

-}
popValue2 : Vm -> Result Error ( Type.Value, Type.Value, Vm )
popValue2 vm =
    case vm.stack of
        (Stack.Value first) :: (Stack.Value second) :: rest ->
            Ok ( first, second, { vm | stack = rest } )

        _ ->
            Err <| Internal InvalidStack


type alias PoppedValues3 =
    { first : Type.Value
    , second : Type.Value
    , third : Type.Value
    , vm : Vm
    }


{-| Pop three values from the stack. Return all three values and the remaining
stack.

Return `Err (Internal InvalidStack)` if the three topmost items are not a
`Stack.Value`.

-}
popValue3 : Vm -> Result Error PoppedValues3
popValue3 vm =
    case vm.stack of
        (Stack.Value first) :: (Stack.Value second) :: (Stack.Value third) :: rest ->
            Ok
                { first = first
                , second = second
                , third = third
                , vm = { vm | stack = rest }
                }

        _ ->
            Err <| Internal InvalidStack


{-| Pop a number of values from the stack. Return a list of values and the
remaining stack.

Return `Err (Internal InvalidStack)` if not enough values are on the stack or
if not all items are a `Stack.Value`.

-}
popValues : Int -> Vm -> Result Error ( List Type.Value, Stack )
popValues n vm =
    let
        stackSize =
            List.length vm.stack
    in
    if stackSize < n then
        Err <| Internal InvalidStack

    else
        List.take n vm.stack
            |> List.map
                (\value ->
                    case value of
                        Stack.Value value_ ->
                            Ok value_

                        _ ->
                            Err <| Internal InvalidStack
                )
            |> List.foldr
                (\value acc ->
                    Result.map2 (\value_ acc_ -> value_ :: acc_) value acc
                )
                (Ok [])
            |> Result.map (\first -> ( first, List.drop n vm.stack ))


parseAndCompileProgram : Parser Program -> String -> Result Error CompiledProgram
parseAndCompileProgram parser =
    Parser.run parser
        >> Result.mapError (always <| Internal ParsingFailed)
        >> Result.map Ast.compileProgram


parseAndEvalInstructions : Vm -> List Type.Value -> Result Error Vm
parseAndEvalInstructions vm instructions =
    let
        parser =
            getParser vm

        compiledProgram =
            instructions
                |> Type.List
                |> Type.toString
                |> parseAndCompileProgram parser

        result =
            compiledProgram
                |> Result.map (Linker.linkProgram vm.compiledFunctions)
                |> Result.map initialize
                |> Result.map (withEnvironment vm.environment)
                |> Result.map run
    in
    -- This code does not check yet for context (whether the caller expects a
    -- value to be returned by `Eval` or not).
    case result of
        Ok (Done subVm) ->
            case subVm.stack of
                ((Stack.Value _) as value) :: _ ->
                    let
                        newVm =
                            { vm | environment = subVm.environment, stack = value :: vm.stack }
                    in
                    Ok newVm

                [] ->
                    vm |> withEnvironment subVm.environment |> Ok

                _ ->
                    Err <| Internal EvalFailed

        _ ->
            Err <| Internal EvalFailed


eval : Vm -> Result Error Vm
eval vm =
    popValue1 vm
        |> Result.andThen
            (\( first, newVm ) ->
                let
                    instructions =
                        case first of
                            Type.List instructions_ ->
                                Ok instructions_

                            Type.Word instruction ->
                                Ok [ Type.Word instruction ]

                            _ ->
                                Err <| Internal InvalidStack
                in
                instructions
                    |> Result.andThen (parseAndEvalInstructions newVm)
                    |> Result.map incrementProgramCounter
            )


{-| Evaluate a primitive that takes 1 argument and put the result on top of the
stack.
-}
eval1 : P.Primitive1 -> Vm -> Result Error Vm
eval1 primitive vm =
    popValue1 vm
        |> Result.andThen
            (\( first, newVm ) ->
                primitive.f first
                    |> Result.map
                        (\value ->
                            newVm
                                |> pushValue1 value
                                |> incrementProgramCounter
                        )
            )


{-| Boolean and arithmetic operators can be called in two ways, for example:

    1 > 0

    greaterp 1 0

Both use the same implementation under the hood, in this case `P.greaterp`,
which always gives the error `WrongInput "greaterp" _`. Since we know the name
of the actual caller, we can correct the error if necessary.

-}
mapWrongInput : String -> Error -> Error
mapWrongInput name error =
    case error of
        WrongInput _ input ->
            WrongInput name input

        otherwise ->
            otherwise


{-| Evaluate a primitive that takes 2 arguments and put the result on top of
the stack.
-}
eval2 : P.Primitive2 -> Vm -> Result Error Vm
eval2 primitive vm =
    popValue2 vm
        |> Result.andThen
            (\( first, second, newVm ) ->
                primitive.f first second
                    |> Result.map
                        (\value ->
                            newVm
                                |> pushValue1 value
                                |> incrementProgramCounter
                        )
                    |> Result.mapError (mapWrongInput primitive.name)
            )


{-| Evaluate a primitive that takes 3 arguments and put the result on top of
the stack.
-}
eval3 : P.Primitive3 -> Vm -> Result Error Vm
eval3 primitive vm =
    popValue3 vm
        |> Result.andThen
            (\result ->
                primitive.f result.first result.second result.third
                    |> Result.map
                        (\value ->
                            result.vm
                                |> pushValue1 value
                                |> incrementProgramCounter
                        )
                    |> Result.mapError (mapWrongInput primitive.name)
            )


{-| Evaluate a primitive that takes n arguments and put the result on top of
the stack.
-}
evalN : P.PrimitiveN -> Int -> Vm -> Result Error Vm
evalN primitive n vm =
    popValues n vm
        |> Result.andThen
            (\( arguments, rest ) ->
                primitive.f arguments
                    |> Result.map
                        (\value ->
                            { vm | stack = Stack.Value value :: rest }
                                |> incrementProgramCounter
                        )
                    |> Result.mapError (mapWrongInput primitive.name)
            )


{-| Run a command that takes no argument.
-}
command0 : C.Command0 -> Vm -> Result Error Vm
command0 command vm =
    command.f vm.environment
        |> Result.map
            (\environment ->
                { vm | environment = environment }
                    |> incrementProgramCounter
            )


{-| Run a command that takes one argument.
-}
command1 : C.Command1 -> Vm -> Result Error Vm
command1 command vm =
    popValue1 vm
        |> Result.andThen
            (\( first, newVm ) ->
                command.f first newVm.environment
                    |> Result.map
                        (\environment ->
                            { newVm | environment = environment }
                                |> incrementProgramCounter
                        )
            )


{-| Run a command that takes two arguments.
-}
command2 : C.Command2 -> Vm -> Result Error Vm
command2 command vm =
    popValue2 vm
        |> Result.andThen
            (\( first, second, newVm ) ->
                command.f first second vm.environment
                    |> Result.map
                        (\environment ->
                            { newVm | environment = environment }
                                |> incrementProgramCounter
                        )
            )


{-| Run a command that takes n arguments.
-}
commandN : C.CommandN -> Int -> Vm -> Result Error Vm
commandN command n vm =
    popValues n vm
        |> Result.andThen
            (\( arguments, rest ) ->
                command.f arguments vm.environment
                    |> Result.map
                        (\environment ->
                            { vm | stack = rest, environment = environment }
                                |> incrementProgramCounter
                        )
                    |> Result.mapError (mapWrongInput command.name)
            )


{-| Put a value representing some internal state of a `Vm` on the stack.
-}
introspect0 : I.Introspect0 -> Vm -> Result Error Vm
introspect0 primitive vm =
    primitive.f vm.scopes
        |> Result.map
            (\value ->
                { vm | stack = Stack.Value value :: vm.stack }
                    |> incrementProgramCounter
            )
        |> Result.mapError (Internal << Scope)


{-| Put a value representing some internal state of a `Vm` on the stack.
-}
introspect1 : I.Introspect1 -> Vm -> Result Error Vm
introspect1 primitive vm =
    popValue1 vm
        |> Result.andThen
            (\( first, newVm ) ->
                primitive.f first newVm.scopes
                    |> Result.map
                        (\value ->
                            newVm
                                |> pushValue1 value
                                |> incrementProgramCounter
                        )
                    |> Result.mapError (Internal << Scope)
            )


pushVariable : String -> Vm -> Result Error Vm
pushVariable name vm =
    case Scope.thing name vm.scopes of
        Just (Defined value) ->
            Ok
                ({ vm | stack = Stack.Value value :: vm.stack } |> incrementProgramCounter)

        _ ->
            Err <| Error.VariableUndefined name


storeVariable : String -> Vm -> Result Error Vm
storeVariable name vm =
    popValue1 vm
        |> Result.map
            (\( first, newVm ) ->
                { newVm | scopes = Scope.make name first vm.scopes }
                    |> incrementProgramCounter
            )


localVariable : String -> Vm -> Result Error Vm
localVariable name vm =
    Ok
        ({ vm | scopes = Scope.local name vm.scopes }
            |> incrementProgramCounter
        )


pushLoopScope : Vm -> Result Error Vm
pushLoopScope vm =
    popValue1 vm
        |> Result.andThen
            (\( first, newVm ) ->
                first
                    |> Type.toInt
                    |> Result.mapError (Internal << Type)
                    |> Result.map
                        (\total ->
                            { newVm | scopes = Scope.pushLoopScope total vm.scopes }
                        )
                    |> Result.map incrementProgramCounter
            )


popLoopScope : Vm -> Result Error Vm
popLoopScope vm =
    vm.scopes
        |> Scope.popLoopScope
        |> Result.map
            (\scopes ->
                { vm | scopes = scopes }
                    |> incrementProgramCounter
            )
        |> Result.mapError (Internal << Scope)


enterLoopScope : Vm -> Result Error Vm
enterLoopScope vm =
    vm.scopes
        |> Scope.enterLoopScope
        |> Result.map
            (\( lastPass, scopes ) ->
                { vm
                    | scopes = scopes
                    , stack = (Type.fromBool lastPass |> Stack.Value) :: vm.stack
                }
                    |> incrementProgramCounter
            )
        |> Result.mapError (Internal << Scope)


pushTemplateScope : Vm -> Result Error Vm
pushTemplateScope vm =
    popValue1 vm
        |> Result.map
            (\( first, newVm ) ->
                { newVm | scopes = Scope.pushTemplateScope first vm.scopes }
                    |> incrementProgramCounter
            )


popTemplateScope : Vm -> Result Error Vm
popTemplateScope vm =
    vm.scopes
        |> Scope.popTemplateScope
        |> Result.map
            (\scopes ->
                { vm | scopes = scopes }
                    |> incrementProgramCounter
            )
        |> Result.mapError (Internal << Scope)


enterTemplateScope : Vm -> Result Error Vm
enterTemplateScope vm =
    vm.scopes
        |> Scope.enterTemplateScope
        |> Result.map
            (\( lastPass, scopes ) ->
                { vm
                    | scopes = scopes
                    , stack = (Type.fromBool lastPass |> Stack.Value) :: vm.stack
                }
                    |> incrementProgramCounter
            )
        |> Result.mapError (Internal << Scope)


pushLocalScope : Vm -> Result Error Vm
pushLocalScope vm =
    case vm.stack of
        (Stack.Address returnAddress) :: rest ->
            Ok
                ({ vm
                    | scopes = Scope.pushLocalScope returnAddress vm.scopes
                    , stack = rest
                 }
                    |> incrementProgramCounter
                )

        _ ->
            Err <| Internal NoReturnAddress


popLocalScope : Vm -> Result Error Vm
popLocalScope vm =
    vm.scopes
        |> Scope.popLocalScope
        |> Result.map
            (\( returnAddress, scopes ) ->
                { vm
                    | scopes = scopes
                    , stack = Stack.Address returnAddress :: vm.stack
                }
                    |> incrementProgramCounter
            )
        |> Result.mapError (Internal << Scope)


jumpIfFalse : Int -> Vm -> Result Error Vm
jumpIfFalse by vm =
    popValue1 vm
        |> Result.andThen
            (\( first, newVm ) ->
                Type.toBool first
                    |> Result.map
                        (\bool ->
                            if not bool then
                                { newVm | programCounter = vm.programCounter + by }

                            else
                                incrementProgramCounter newVm
                        )
                    |> Result.mapError (Internal << Type)
            )


jumpIfTrue : Int -> Vm -> Result Error Vm
jumpIfTrue by vm =
    popValue1 vm
        |> Result.andThen
            (\( first, newVm ) ->
                Type.toBool first
                    |> Result.map
                        (\bool ->
                            if bool then
                                { newVm | programCounter = vm.programCounter + by }

                            else
                                incrementProgramCounter newVm
                        )
                    |> Result.mapError (Internal << Type)
            )


return : Vm -> Result Error Vm
return vm =
    case vm.stack of
        (Stack.Address returnAddress) :: rest ->
            Ok
                { vm
                    | programCounter = returnAddress
                    , stack = rest
                }

        _ ->
            Err <| Internal NoReturnAddress


duplicate : Vm -> Result Error Vm
duplicate vm =
    popValue1 vm
        |> Result.map
            (\( first, newVm ) ->
                newVm
                    |> pushValue1 first
                    |> pushValue1 first
                    |> incrementProgramCounter
            )


flip : Vm -> Result Error Vm
flip vm =
    case vm.stack of
        (Stack.Value first) :: (Stack.Value second) :: rest ->
            Ok
                ({ vm
                    | stack =
                        Stack.Value second
                            :: Stack.Value first
                            :: rest
                 }
                    |> incrementProgramCounter
                )

        _ ->
            Err <| Internal InvalidStack


raise : Exception -> Vm -> Result Error Vm
raise exception vm =
    case exception of
        Exception.WrongInput function ->
            popValue1 vm
                |> Result.andThen
                    (\( first, _ ) ->
                        Err <| WrongInput function (Type.toDebugString first)
                    )

        Exception.NoUseOfValue ->
            popValue1 vm
                |> Result.andThen
                    (\( first, _ ) ->
                        Err <| NoUseOfValue (Type.toDebugString first)
                    )

        Exception.NoOutput caller callee ->
            Err <| NoOutput caller callee

        Exception.OutputOutsideFunction ->
            Err <| OutputOutsideFunction

        Exception.NotEnoughInputs callable ->
            Err <| NotEnoughInputs callable

        Exception.TooManyInputs callable ->
            Err <| TooManyInputs callable

        Exception.FunctionAlreadyDefined functionName ->
            Err <| FunctionAlreadyDefined functionName

        Exception.CallableUndefined functionName ->
            Err <| CallableUndefined functionName


{-| Execute a single instruction.
-}
execute : Instruction -> Vm -> Result Error Vm
execute instruction vm =
    case instruction of
        PushValue value ->
            Ok
                ({ vm | stack = Stack.Value value :: vm.stack } |> incrementProgramCounter)

        PushVariable name ->
            pushVariable name vm

        StoreVariable name ->
            storeVariable name vm

        LocalVariable name ->
            localVariable name vm

        Introspect0 primitive ->
            introspect0 primitive vm

        Introspect1 primitive ->
            introspect1 primitive vm

        Eval ->
            eval vm

        Eval1 primitive ->
            eval1 primitive vm

        Eval2 primitive ->
            eval2 primitive vm

        Eval3 primitive ->
            eval3 primitive vm

        EvalN primitive n ->
            evalN primitive n vm

        Command0 command ->
            command0 command vm

        Command1 command ->
            command1 command vm

        Command2 command ->
            command2 command vm

        CommandN command n ->
            commandN command n vm

        PushLoopScope ->
            pushLoopScope vm

        PopLoopScope ->
            popLoopScope vm

        EnterLoopScope ->
            enterLoopScope vm

        PushTemplateScope ->
            pushTemplateScope vm

        PopTemplateScope ->
            popTemplateScope vm

        EnterTemplateScope ->
            enterTemplateScope vm

        PushLocalScope ->
            pushLocalScope vm

        PopLocalScope ->
            popLocalScope vm

        JumpIfFalse by ->
            jumpIfFalse by vm

        JumpIfTrue by ->
            jumpIfTrue by vm

        Jump by ->
            Ok { vm | programCounter = vm.programCounter + by }

        Call address ->
            Ok
                { vm
                    | stack = Stack.Address (vm.programCounter + 1) :: vm.stack
                    , programCounter = address
                }

        CallByName functionName ->
            Dict.get functionName vm.functionTable
                |> Result.fromMaybe (Internal <| FunctionUndefined functionName)
                |> Result.map
                    (\address ->
                        { vm
                            | stack = Stack.Address (vm.programCounter + 1) :: vm.stack
                            , programCounter = address
                        }
                    )

        PushVoid ->
            Ok ({ vm | stack = Stack.Void :: vm.stack } |> incrementProgramCounter)

        Return ->
            return vm

        CheckReturn ->
            case vm.stack of
                Stack.Void :: rest ->
                    Ok
                        ({ vm | stack = Stack.Value Type.false :: rest }
                            |> incrementProgramCounter
                        )

                (Stack.Value _) :: _ ->
                    Ok
                        ({ vm | stack = Stack.Value Type.true :: vm.stack }
                            |> incrementProgramCounter
                        )

                _ ->
                    Err <| Internal InvalidStack

        Duplicate ->
            duplicate vm

        Flip ->
            flip vm

        Raise exception ->
            raise exception vm


type State
    = Paused Vm
    | Done Vm


{-| Execute a single instruction.
-}
step : Vm -> State
step vm =
    let
        instruction =
            Array.get vm.programCounter vm.instructions
    in
    case instruction of
        Just instruction_ ->
            case execute instruction_ vm of
                Ok newVm ->
                    Paused newVm

                Err error ->
                    Done { vm | environment = Environment.error (Error.toString error) vm.environment }

        _ ->
            Done vm


{-| Run a `Vm` until the program counter points to an invalid instruction.

This version gets tail-call optimized by using a helper function that either
returns a value or calls itself.

-}
run : Vm -> State
run vm =
    let
        run_ result =
            case result of
                Ok newVm ->
                    let
                        instruction =
                            Array.get newVm.programCounter newVm.instructions
                    in
                    case instruction of
                        Just instruction_ ->
                            run_ (execute instruction_ newVm)

                        _ ->
                            Done newVm

                Err error ->
                    Done { vm | environment = Environment.error (Error.toString error) vm.environment }
    in
    run_ (Ok vm)
