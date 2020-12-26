module Vm.Introspect exposing
    ( Introspect0
    , Introspect1
    , repcount
    , templateVariable
    )

{-| This module provides primitives for accessing the current state of template
and loop scopes in a virtual machine.
-}

import Vm.Scope as Scope exposing (Scope)
import Vm.Type as Type


type alias Introspect0 =
    { name : String
    , f : List Scope -> Result Scope.Error Type.Value
    }


type alias Introspect1 =
    { name : String
    , f : Type.Value -> List Scope -> Result Scope.Error Type.Value
    }


repcount : List Scope -> Result Scope.Error Type.Value
repcount scopes =
    Ok <| Type.Int (Scope.repcount scopes)


templateVariable : Type.Value -> List Scope -> Result Scope.Error Type.Value
templateVariable value scopes =
    Scope.templateVariable value scopes
