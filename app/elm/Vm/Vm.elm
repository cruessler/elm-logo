module Vm.Vm exposing (..)

{-| This module provides types and data structures for representing a virtual
machine as well as functions for running it.
-}

import Array exposing (Array)
import Dict exposing (Dict)
import Environment exposing (Environment)
import Vm.Command as C
import Vm.Error as Error exposing (Error(..), Internal(..))
import Vm.Exception as Exception exposing (Exception)
import Vm.Introspect as I
import Vm.Primitive as P
import Vm.Scope as Scope exposing (Scope, Binding(..))
import Vm.Stack as Stack exposing (Stack)
import Vm.Type as Type


{-| Represent instructions a `Vm` can execute.
-}
type Instruction
    = PushValue Type.Value
    | PushVariable String
    | StoreVariable String
    | LocalVariable String
    | Introspect0 (I.Introspect0 Vm)
    | Introspect1 (I.Introspect1 Vm)
    | Eval1 P.Primitive1
    | Eval2 P.Primitive2
    | Command1 C.Command1
    | Command2 C.Command2
    | PushLoopScope
    | EnterLoopScope
    | PopLoopScope
    | PushTemplateScope
    | EnterTemplateScope
    | PopTemplateScope
    | PushLocalScope
    | PopLocalScope
    | JumpIfFalse Int
    | JumpIfTrue Int
    | Jump Int
    | Call Int
    | CallByName String
    | PushVoid
    | Return
    | CheckReturn
    | Duplicate
    | Raise Exception


{-| Represent a stack based virtual machine.
-}
type alias Vm =
    { instructions : Array Instruction
    , programCounter : Int
    , stack : Stack
    , scopes : List Scope
    , environment : Environment
    , functionTable : Dict String Int
    }


{-| Initialize a `Vm` with a list of instructions and a program counter.
-}
initialize : List Instruction -> Dict String Int -> Int -> Vm
initialize instructions functionTable programCounter =
    { instructions = Array.fromList instructions
    , programCounter = programCounter
    , stack = []
    , scopes = Scope.empty
    , environment = Environment.empty
    , functionTable = functionTable
    }


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
                        { vm | stack = (Stack.Value value :: rest) }
                            |> incrementProgramCounter
                    )

        _ :: rest ->
            Err <| Internal InvalidStack

        _ ->
            Err <| NotEnoughInputs <| primitive.name


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
                        { vm | stack = (Stack.Value value :: rest) }
                            |> incrementProgramCounter
                    )

        _ :: _ :: rest ->
            Err <| Internal InvalidStack

        _ ->
            Err <| NotEnoughInputs <| primitive.name


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

        _ :: rest ->
            Err <| Internal InvalidStack

        _ ->
            Err <| NotEnoughInputs <| command.name


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

        _ :: _ :: rest ->
            Err <| Internal InvalidStack

        _ ->
            Err <| NotEnoughInputs <| command.name


{-| Put a value representing some internal state of a `Vm` on the stack.
-}
introspect0 : I.Introspect0 Vm -> Vm -> Result Error Vm
introspect0 primitive vm =
    primitive.f vm
        |> Result.map
            (\value ->
                { vm | stack = Stack.Value value :: vm.stack }
                    |> incrementProgramCounter
            )
        |> Result.mapError (Internal << Scope)


{-| Put a value representing some internal state of a `Vm` on the stack.
-}
introspect1 : I.Introspect1 Vm -> Vm -> Result Error Vm
introspect1 primitive vm =
    case vm.stack of
        (Stack.Value first) :: rest ->
            primitive.f first vm
                |> Result.map
                    (\value ->
                        { vm | stack = Stack.Value value :: rest }
                            |> incrementProgramCounter
                    )
                |> Result.mapError (Internal << Scope)

        _ :: rest ->
            Err <| Internal InvalidStack

        _ ->
            Err <| NotEnoughInputs primitive.name


pushVariable : String -> Vm -> Result Error Vm
pushVariable name vm =
    case Scope.thing name vm.scopes of
        Just (Defined value) ->
            Ok
                ({ vm | stack = Stack.Value value :: vm.stack } |> incrementProgramCounter)

        _ ->
            Err <| Internal <| Error.VariableUndefined name


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

        _ :: rest ->
            Err <| Internal InvalidStack

        _ ->
            Err <| Internal EmptyStack


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
            Err <| Internal EmptyStack


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

        _ :: rest ->
            Err <| Internal InvalidStack

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

        _ :: rest ->
            Err <| Internal InvalidStack

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
                    , stack = (Stack.Address returnAddress) :: vm.stack
                }
                    |> incrementProgramCounter
            )
        |> Result.mapError (Internal << Scope)


toBoolean : Type.Value -> Result Error Bool
toBoolean value =
    case value of
        Type.Word word ->
            if String.toLower word == "true" then
                Ok True
            else if String.toLower word == "false" then
                Ok False
            else
                Err <| Internal <| NoBoolean (toString value)

        _ ->
            Err <| Internal <| NoBoolean (toString value)


jumpIfFalse : Int -> Vm -> Result Error Vm
jumpIfFalse by vm =
    case vm.stack of
        (Stack.Value first) :: rest ->
            toBoolean first
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

        _ :: rest ->
            Err <| Internal InvalidStack

        _ ->
            Err <| Internal EmptyStack


jumpIfTrue : Int -> Vm -> Result Error Vm
jumpIfTrue by vm =
    case vm.stack of
        (Stack.Value first) :: rest ->
            toBoolean first
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

        _ :: rest ->
            Err <| Internal InvalidStack

        _ ->
            Err <| Internal EmptyStack


return : Vm -> Result Error Vm
return vm =
    case vm.stack of
        (Stack.Address returnAddress) :: rest ->
            Ok
                { vm
                    | programCounter = returnAddress
                    , stack = rest
                }

        _ :: rest ->
            Err <| Internal InvalidStack

        _ ->
            Err <| Internal NoReturnAddress


duplicate : Vm -> Result Error Vm
duplicate vm =
    case vm.stack of
        (Stack.Value first) :: rest ->
            Ok
                ({ vm
                    | stack =
                        (Stack.Value first)
                            :: (Stack.Value first)
                            :: rest
                 }
                    |> incrementProgramCounter
                )

        _ ->
            Err <| Internal EmptyStack


raise : Exception -> Vm -> Result Error Vm
raise exception vm =
    case exception of
        Exception.WrongInput function ->
            case vm.stack of
                (Stack.Value first) :: rest ->
                    Err <| WrongInput function (Type.toString first)

                _ ->
                    Err <| Internal EmptyStack

        Exception.NoUseOfValue ->
            case vm.stack of
                (Stack.Value first) :: rest ->
                    Err <| NoUseOfValue (Type.toString first)

                _ ->
                    Err <| Internal EmptyStack

        _ ->
            Err <| Exception exception


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
                        ({ vm | stack = (Stack.Value Type.false) :: rest }
                            |> incrementProgramCounter
                        )

                (Stack.Value _) :: rest ->
                    Ok
                        ({ vm | stack = (Stack.Value Type.true) :: vm.stack }
                            |> incrementProgramCounter
                        )

                _ ->
                    Err <| Internal InvalidStack

        Duplicate ->
            duplicate vm

        Raise exception ->
            raise exception vm


{-| Execute a single instruction, returning an error when the program counter
does not point to a valid instruction.
-}
step : Vm -> Result Error Vm
step vm =
    Array.get vm.programCounter vm.instructions
        |> Result.fromMaybe (Internal NoInstruction)
        |> Result.andThen
            (\instruction -> execute instruction vm)


{-| Run a `Vm` until the program counter points to an invalid instruction.
-}
run : Vm -> Result Error Vm
run vm =
    case Array.get vm.programCounter vm.instructions of
        Just instruction ->
            execute instruction vm
                |> Result.andThen (\vm -> run vm)

        _ ->
            Ok vm
