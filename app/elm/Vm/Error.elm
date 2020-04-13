module Vm.Error exposing (Error(..), Internal(..), toString)

{-| This module provides types for representing errors and invalid states.
-}

import Vm.Scope as Scope
import Vm.Type as Type


{-| Represent errors that stem from the specifics of this Logo implementation.
These include invalid states of the VM’s stack or instruction pointer, and
calls to undefined functions.
-}
type Internal
    = InvalidStack
    | FunctionUndefined String
    | Scope Scope.Error
    | Type Type.Error
    | NoIterator
    | NoReturnAddress
    | NoInstruction


type Error
    = WrongInput String String
    | NoUseOfValue String
    | NoOutput String String
    | VariableUndefined String
    | Internal Internal
    | OutputOutsideFunction
    | NotEnoughInputs String
    | TooManyInputs String


toString : Error -> String
toString error =
    case error of
        WrongInput caller input ->
            caller ++ " doesn’t like " ++ input ++ " as input"

        NoUseOfValue value ->
            "You don’t say what to do with " ++ value

        NoOutput caller callee ->
            callee ++ " did not output to " ++ caller

        VariableUndefined name ->
            name ++ " has no value"

        OutputOutsideFunction ->
            "Can only use output inside a procedure"

        NotEnoughInputs callable ->
            "not enough inputs to " ++ callable

        TooManyInputs callable ->
            "too many inputs to " ++ callable

        Internal _ ->
            "Logo: Fatal Internal Error."
