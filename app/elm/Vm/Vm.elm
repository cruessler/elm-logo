module Vm.Vm exposing (..)

{-| This module provides types and data structures for representing a virtual
machine as well as functions for running it.
-}

import Array exposing (Array)
import Environment exposing (Environment)
import Vm.Command as C
import Vm.Introspect as I
import Vm.Primitive as P
import Vm.Scope as Scope exposing (Scope, Binding(..))
import Vm.Type exposing (Value(..))


{-| Represent instructions a `Vm` can execute.
-}
type Instruction
    = PushValue Value
    | PushVariable String
    | StoreVariable String
    | Introspect0 (I.Introspect0 Vm)
    | Eval1 P.Primitive1
    | Eval2 P.Primitive2
    | Command1 C.Command1
    | Command2 C.Command2
    | PushLoopScope
    | EnterLoopScope
    | PopLoopScope


{-| Represent a stack based virtual machine.
-}
type alias Vm =
    { instructions : Array Instruction
    , programCounter : Int
    , stack : List Value
    , scopes : List Scope
    , environment : Environment
    }


{-| Increment the program counter of a `Vm`.
-}
incrementProgramCounter : Vm -> Vm
incrementProgramCounter vm =
    { vm | programCounter = vm.programCounter + 1 }


{-| Evaluate a primitive that takes 1 argument and put the result on top of the
stack.
-}
eval1 : P.Primitive1 -> Vm -> Result String Vm
eval1 primitive vm =
    case vm.stack of
        first :: rest ->
            primitive.f first
                |> Result.map
                    (\value ->
                        { vm | stack = (value :: rest) }
                            |> incrementProgramCounter
                    )

        _ ->
            Err <| "Not enough inputs to " ++ primitive.name


{-| Evaluate a primitive that takes 2 arguments and put the result on top of
the stack.
-}
eval2 : P.Primitive2 -> Vm -> Result String Vm
eval2 primitive vm =
    case vm.stack of
        first :: second :: rest ->
            primitive.f first second
                |> Result.map
                    (\value ->
                        { vm | stack = (value :: rest) }
                            |> incrementProgramCounter
                    )

        _ ->
            Err <| "Not enough inputs to " ++ primitive.name


{-| Run a command that takes one argument.
-}
command1 : C.Command1 -> Vm -> Result String Vm
command1 command vm =
    case vm.stack of
        first :: rest ->
            command.f first vm.environment
                |> Result.map
                    (\environment ->
                        { vm | stack = rest, environment = environment }
                            |> incrementProgramCounter
                    )

        _ ->
            Err <| "Not enough inputs to " ++ command.name


{-| Run a command that takes two arguments.
-}
command2 : C.Command2 -> Vm -> Result String Vm
command2 command vm =
    case vm.stack of
        first :: second :: rest ->
            command.f first second vm.environment
                |> Result.map
                    (\environment ->
                        { vm | stack = rest, environment = environment }
                            |> incrementProgramCounter
                    )

        _ ->
            Err <| "Not enough inputs to " ++ command.name


{-| Put a value representing some internal state of a `Vm` on the stack.
-}
introspect0 : I.Introspect0 Vm -> Vm -> Result String Vm
introspect0 primitive vm =
    primitive.f vm
        |> Result.map
            (\value ->
                { vm | stack = value :: vm.stack }
                    |> incrementProgramCounter
            )


pushVariable : String -> Vm -> Result String Vm
pushVariable name vm =
    case Scope.thing name vm.scopes of
        Just (Defined value) ->
            Ok
                ({ vm | stack = value :: vm.stack } |> incrementProgramCounter)

        _ ->
            Err <| name ++ " has no value"


storeVariable : String -> Vm -> Result String Vm
storeVariable name vm =
    case vm.stack of
        first :: rest ->
            Ok
                ({ vm
                    | stack = rest
                    , scopes = Scope.make name first vm.scopes
                 }
                    |> incrementProgramCounter
                )

        _ ->
            Err <| "The stack is empty"


pushLoop : Vm -> Vm
pushLoop vm =
    { vm | scopes = Scope.pushLoopScope vm.scopes }
        |> incrementProgramCounter


popLoopScope : Vm -> Result String Vm
popLoopScope vm =
    vm.scopes
        |> Scope.popLoopScope
        |> Result.map
            (\scopes ->
                { vm | scopes = scopes }
                    |> incrementProgramCounter
            )


enterLoopScope : Vm -> Result String Vm
enterLoopScope vm =
    vm.scopes
        |> Scope.enterLoopScope
        |> Result.map
            (\scopes ->
                { vm | scopes = scopes }
                    |> incrementProgramCounter
            )


{-| Execute a single instruction.
-}
execute : Instruction -> Vm -> Result String Vm
execute instruction vm =
    case instruction of
        PushValue value ->
            Ok
                ({ vm | stack = value :: vm.stack } |> incrementProgramCounter)

        PushVariable name ->
            pushVariable name vm

        StoreVariable name ->
            storeVariable name vm

        Introspect0 primitive ->
            introspect0 primitive vm

        Eval1 primitive ->
            eval1 primitive vm

        Eval2 primitive ->
            eval2 primitive vm

        Command1 command ->
            command1 command vm

        Command2 command ->
            command2 command vm

        PushLoopScope ->
            Ok (pushLoopScope vm)

        PopLoopScope ->
            popLoopScope vm

        EnterLoopScope ->
            enterLoopScope vm


{-| Execute a single instruction, returning an error when the program counter
does not point to a valid instruction.
-}
step : Vm -> Result String Vm
step vm =
    Array.get vm.programCounter vm.instructions
        |> Result.fromMaybe ("Program counter pointing to non-existent instruction")
        |> Result.andThen
            (\instruction -> execute instruction vm)


{-| Run a `Vm` until the program counter points to an invalid instruction.
-}
run : Vm -> Result String Vm
run vm =
    case Array.get vm.programCounter vm.instructions of
        Just instruction ->
            execute instruction vm
                |> Result.andThen (\vm -> run vm)

        _ ->
            Ok vm
