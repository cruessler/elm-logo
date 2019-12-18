module Vm.Error exposing (Error(..), Internal(..))

{-| This module provides types for representing errors and invalid states.
-}

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
    | Internal Internal
