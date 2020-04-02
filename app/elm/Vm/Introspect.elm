module Vm.Introspect exposing
    ( Introspect0
    , Introspect1
    , repcount
    , templateVariable
    )

{-| This module provides primitives for accessing the current state of the
virtual machine.

It cannot import the `Vm` type as that would form a circular dependency: one of
the virtual machine’s instructions depends on the first type alias.

Thus, all types are parametrized.

-}

import Vm.Scope as Scope exposing (Scope)
import Vm.Type as Type


type alias Introspect0 a =
    { name : String
    , f : a -> Result Scope.Error Type.Value
    }


type alias Introspect1 a =
    { name : String
    , f : Type.Value -> a -> Result Scope.Error Type.Value
    }


repcount : { a | scopes : List Scope } -> Result Scope.Error Type.Value
repcount vm =
    Ok <| Type.Word (Scope.repcount vm.scopes |> String.fromInt)


templateVariable : Type.Value -> { a | scopes : List Scope } -> Result Scope.Error Type.Value
templateVariable value vm =
    Scope.templateVariable value vm.scopes
