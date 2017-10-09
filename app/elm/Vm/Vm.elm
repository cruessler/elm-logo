module Vm.Vm exposing (..)

{-| This module provides types and data structures for representing a virtual
machine.
-}

import Array exposing (Array)
import Vm.Primitives as P
import Vm.Types exposing (Value(..))


{-| Represent instructions a `Vm` can execute.
-}
type Instruction
    = PutValue Value
    | Eval1 P.Primitive1


{-| Represent a stack based virtual machine.
-}
type alias Vm =
    { instructions : Array Instruction
    , programCounter : Int
    , stack : List Value
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


{-| Execute a single instruction.
-}
execute : Instruction -> Vm -> Result String Vm
execute instruction vm =
    case instruction of
        PutValue value ->
            Ok
                ({ vm | stack = value :: vm.stack } |> incrementProgramCounter)

        Eval1 primitive ->
            eval1 primitive vm


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
