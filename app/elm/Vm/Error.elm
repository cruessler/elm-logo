module Vm.Error exposing (Error(..), Internal(..), toString)

{-| This module provides types for representing errors and invalid states.
-}

import Vm.Exception as Exception exposing (Exception)
import Vm.Scope as Scope
import Vm.Type as Type


{-| Represent errors that stem from the specifics of this Logo implementation.
These include invalid states of the VM’s stack or instruction pointer, and
calls to undefined functions.
-}
type Internal
    = EmptyStack
    | InvalidStack
    | VariableUndefined String
    | FunctionUndefined String
    | Scope Scope.Error
    | Type Type.Error
    | NoIterator
    | NoReturnAddress
    | NoBoolean String
    | NoInstruction


type Error
    = NotEnoughInputs String
    | Primitive String
    | WrongInput String String
    | NoUseOfValue String
    | NoOutput String String
    | Internal Internal
    | Exception Exception


toString : Error -> String
toString error =
    case error of
        WrongInput caller input ->
            caller ++ " doesn’t like " ++ input ++ " as input"

        NoUseOfValue value ->
            "You don’t say what to do with " ++ value

        NoOutput caller callee ->
            callee ++ " did not output to " ++ caller

        Exception exception ->
            Exception.toString exception

        _ ->
            Debug.crash "unimplemented"
