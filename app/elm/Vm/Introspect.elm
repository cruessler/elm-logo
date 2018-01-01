module Vm.Introspect exposing (Introspect0, repcount)

{-| This module provides primitives for accessing the current state of the
virtual machine.

It cannot import the `Vm` type as that would form a circular dependency: one of
the virtual machine’s instructions depends on the first type alias.

Thus, all types are parametrized.

-}

import Vm.Scope as Scope exposing (Scope)
import Vm.Type exposing (Value(..))


type alias Introspect0 a =
    { name : String
    , f : a -> Result String Value
    }


type alias Introspect1 a =
    { name : String
    , f : Value -> a -> Result String Value
    }


repcount : { a | scopes : List Scope } -> Result String Value
repcount vm =
    Ok <| Word (Scope.repcount vm.scopes |> toString)
