module Vm.Scope
    exposing
        ( Scope(..)
        , Binding(..)
        , empty
        , make
        , thing
        , pushLocalScope
        , local
        )

{-| This module contains types and functions related to Logo’s handling of
variables.

Logo uses [dynamic scoping][dynamic-scoping]. Each function has its own local
scope; when Logo looks for the value of a variable it starts at the innermost
local scope (the one associated with the currently executed function) and works
its way up to the root scope.

The current implementation is not optimized for speed, but for simplicity.

[dynamic-scoping]: https://en.wikipedia.org/wiki/Scope_(computer_science)#Dynamic_scoping

-}

import Dict exposing (Dict)
import Vm.Type exposing (Value)


type Binding
    = Undefined
    | Defined Value


type alias Variables =
    { variables : Dict String Binding }


type Scope
    = Root Variables
    | Local Variables


empty : List Scope
empty =
    [ Root { variables = Dict.empty } ]


member : String -> Scope -> Bool
member name scope =
    case scope of
        Root { variables } ->
            Dict.member name variables

        Local { variables } ->
            Dict.member name variables


get : String -> Scope -> Maybe Binding
get name scope =
    case scope of
        Root { variables } ->
            Dict.get name variables

        Local { variables } ->
            Dict.get name variables


set : String -> Value -> Scope -> Scope
set name value scope =
    case scope of
        Root { variables } ->
            Root { variables = Dict.insert name (Defined value) variables }

        Local { variables } ->
            Local { variables = Dict.insert name (Defined value) variables }


{-| Set the value of the variable named `name` to `value`.

If no variable named `name` exists a new one is created at the root scope.

-}
make : String -> Value -> List Scope -> List Scope
make name value scopes =
    case scopes of
        [ (Root _) as root ] ->
            [ set name value root ]

        first :: rest ->
            if member name first then
                (set name value first) :: rest
            else
                first :: make name value rest

        [] ->
            scopes


{-| Get the current value of the variable named `name`. If there is more than
one such variable the innermost local value is chosen.

Return `Just (Defined _)` if the variable has been created and has a value
associated with it.

Return `Just Undefined` if the variable has been created using `local`, but
has no value associated with it.

Return `Nothing` when there is no value associated with `name`.

-}
thing : String -> List Scope -> Maybe Binding
thing name scopes =
    case scopes of
        first :: rest ->
            if member name first then
                get name first
            else
                thing name rest

        [] ->
            Nothing


{-| Create a new local scope.
-}
pushLocalScope : List Scope -> List Scope
pushLocalScope scopes =
    Local { variables = Dict.empty } :: scopes


{-| Create a variable with local scope. Variables with local scope are visible
to the current function as well as to every function invoked by the current
function.

Overwrites any existing binding with `Undefined`.

-}
local : String -> List Scope -> List Scope
local name scopes =
    case scopes of
        (Local { variables }) :: rest ->
            Local { variables = (Dict.insert name Undefined variables) }
                :: rest

        _ ->
            scopes
