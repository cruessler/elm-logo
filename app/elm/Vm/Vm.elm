module Vm.Vm exposing
    ( State(..)
    , Vm
    , empty
    , initialize
    , run
    , step
    , toValue
    , withEnvironment
    )

{-| This module provides types and data structures for representing a virtual
machine as well as functions for running it.
-}

import Array exposing (Array)
import Compiler.Ast exposing (CompiledFunction)
import Compiler.Linker exposing (LinkedProgram)
import Dict exposing (Dict)
import Environment exposing (Environment)
import Json.Encode as E
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


withEnvironment : Environment -> Vm -> Vm
withEnvironment environment vm =
    { vm
        | environment = environment
    }


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

                Eval1 { name } ->
                    "Eval1 " ++ name

                Eval2 { name } ->
                    "Eval2 " ++ name

                EvalN { name } n ->
                    "EvalN [" ++ String.fromInt n ++ "] " ++ name

                Command0 { name } ->
                    "Command0 " ++ name

                Command1 { name } ->
                    "Command1 " ++ name

                Command2 { name } ->
                    "Command2 " ++ name

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


{-| Evaluate a primitive that takes 1 argument and put the result on top of the
stack.
-}
eval1 : P.Primitive1 -> Vm -> Result Error Vm
eval1 primitive vm =
    case vm.stack of
        (Stack.Value first) :: rest ->
            primitive.f first
                |> Result.map
                    (\value ->
                        { vm | stack = Stack.Value value :: rest }
                            |> incrementProgramCounter
                    )

        _ ->
            Err <| Internal InvalidStack


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
    case vm.stack of
        (Stack.Value first) :: (Stack.Value second) :: rest ->
            primitive.f first second
                |> Result.map
                    (\value ->
                        { vm | stack = Stack.Value value :: rest }
                            |> incrementProgramCounter
                    )
                |> Result.mapError (mapWrongInput primitive.name)

        _ ->
            Err <| Internal InvalidStack


evalN : P.PrimitiveN -> Int -> Vm -> Result Error Vm
evalN primitive n vm =
    let
        stackSize =
            List.length vm.stack
    in
    if stackSize < n then
        Err <| Internal InvalidStack

    else
        let
            arguments =
                List.take n vm.stack
                    |> List.map
                        (\value ->
                            case value of
                                Stack.Value value_ ->
                                    Ok value_

                                _ ->
                                    Err <| Internal InvalidStack
                        )
                    |> List.foldl
                        (\value acc ->
                            Result.map2 (\value_ acc_ -> value_ :: acc_) value acc
                        )
                        (Ok [])

            rest =
                List.drop n vm.stack
        in
        arguments
            |> Result.mapError (always <| Internal InvalidStack)
            |> Result.andThen
                (\arguments_ ->
                    primitive.f arguments_
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
    case vm.stack of
        (Stack.Value first) :: rest ->
            command.f first vm.environment
                |> Result.map
                    (\environment ->
                        { vm | stack = rest, environment = environment }
                            |> incrementProgramCounter
                    )

        _ ->
            Err <| Internal InvalidStack


{-| Run a command that takes two arguments.
-}
command2 : C.Command2 -> Vm -> Result Error Vm
command2 command vm =
    case vm.stack of
        (Stack.Value first) :: (Stack.Value second) :: rest ->
            command.f first second vm.environment
                |> Result.map
                    (\environment ->
                        { vm | stack = rest, environment = environment }
                            |> incrementProgramCounter
                    )

        _ ->
            Err <| Internal InvalidStack


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
    case vm.stack of
        (Stack.Value first) :: rest ->
            primitive.f first vm.scopes
                |> Result.map
                    (\value ->
                        { vm | stack = Stack.Value value :: rest }
                            |> incrementProgramCounter
                    )
                |> Result.mapError (Internal << Scope)

        _ ->
            Err <| Internal InvalidStack


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
    case vm.stack of
        (Stack.Value first) :: rest ->
            Ok
                ({ vm
                    | stack = rest
                    , scopes = Scope.make name first vm.scopes
                 }
                    |> incrementProgramCounter
                )

        _ ->
            Err <| Internal InvalidStack


localVariable : String -> Vm -> Result Error Vm
localVariable name vm =
    Ok
        ({ vm | scopes = Scope.local name vm.scopes }
            |> incrementProgramCounter
        )


pushLoopScope : Vm -> Result Error Vm
pushLoopScope vm =
    case vm.stack of
        (Stack.Value first) :: rest ->
            first
                |> Type.toInt
                |> Result.mapError (Internal << Type)
                |> Result.map
                    (\total ->
                        { vm
                            | scopes = Scope.pushLoopScope total vm.scopes
                            , stack = rest
                        }
                    )
                |> Result.map incrementProgramCounter

        _ ->
            Err <| Internal InvalidStack


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
    case vm.stack of
        (Stack.Value first) :: rest ->
            Ok
                ({ vm
                    | scopes = Scope.pushTemplateScope first vm.scopes
                    , stack = rest
                 }
                    |> incrementProgramCounter
                )

        _ ->
            Err <| Internal NoIterator


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
    case vm.stack of
        (Stack.Value first) :: rest ->
            Type.toBool first
                |> Result.map
                    (\bool ->
                        if not bool then
                            { vm
                                | stack = rest
                                , programCounter = vm.programCounter + by
                            }

                        else
                            { vm | stack = rest } |> incrementProgramCounter
                    )
                |> Result.mapError (Internal << Type)

        _ ->
            Err <| Internal InvalidStack


jumpIfTrue : Int -> Vm -> Result Error Vm
jumpIfTrue by vm =
    case vm.stack of
        (Stack.Value first) :: rest ->
            Type.toBool first
                |> Result.map
                    (\bool ->
                        if bool then
                            { vm
                                | stack = rest
                                , programCounter = vm.programCounter + by
                            }

                        else
                            { vm | stack = rest } |> incrementProgramCounter
                    )
                |> Result.mapError (Internal << Type)

        _ ->
            Err <| Internal InvalidStack


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
    case vm.stack of
        (Stack.Value first) :: rest ->
            Ok
                ({ vm
                    | stack =
                        Stack.Value first
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
            case vm.stack of
                (Stack.Value first) :: _ ->
                    Err <| WrongInput function (Type.toDebugString first)

                _ ->
                    Err <| Internal InvalidStack

        Exception.NoUseOfValue ->
            case vm.stack of
                (Stack.Value first) :: _ ->
                    Err <| NoUseOfValue (Type.toDebugString first)

                _ ->
                    Err <| Internal InvalidStack

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

        Eval1 primitive ->
            eval1 primitive vm

        Eval2 primitive ->
            eval2 primitive vm

        EvalN primitive n ->
            evalN primitive n vm

        Command0 command ->
            command0 command vm

        Command1 command ->
            command1 command vm

        Command2 command ->
            command2 command vm

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
