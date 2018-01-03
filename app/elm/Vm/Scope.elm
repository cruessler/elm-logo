module Vm.Scope
    exposing
        ( Scope(..)
        , Binding(..)
        , empty
        , make
        , thing
        , pushLocalScope
        , popLocalScope
        , local
        , pushLoopScope
        , popLoopScope
        , enterLoopScope
        , repcount
        , pushTemplateScope
        , popTemplateScope
        , enterTemplateScope
        , templateVariable
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
import Vm.Iterator as Iterator exposing (Iterator)
import Vm.Type as Type


type Binding
    = Undefined
    | Defined Type.Value


type alias Variables =
    { variables : Dict String Binding }


type Scope
    = Root Variables
    | Local Int Variables
    | Template Iterator
    | Loop Int


empty : List Scope
empty =
    [ Root { variables = Dict.empty } ]


member : String -> Scope -> Bool
member name scope =
    case scope of
        Root { variables } ->
            Dict.member name variables

        Local _ { variables } ->
            Dict.member name variables

        _ ->
            False


get : String -> Scope -> Maybe Binding
get name scope =
    case scope of
        Root { variables } ->
            Dict.get name variables

        Local _ { variables } ->
            Dict.get name variables

        _ ->
            Nothing


set : String -> Type.Value -> Scope -> Scope
set name value scope =
    case scope of
        Root { variables } ->
            Root { variables = Dict.insert name (Defined value) variables }

        Local returnAddress { variables } ->
            Local returnAddress { variables = Dict.insert name (Defined value) variables }

        _ ->
            scope


{-| Set the value of the variable named `name` to `value`.

If no variable named `name` exists a new one is created at the root scope.

-}
make : String -> Type.Value -> List Scope -> List Scope
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
pushLocalScope : Int -> List Scope -> List Scope
pushLocalScope returnAddress scopes =
    Local returnAddress { variables = Dict.empty } :: scopes


popLocalScope : List Scope -> Result String ( Int, List Scope )
popLocalScope scopes =
    case scopes of
        (Local returnAddress _) :: rest ->
            Ok ( returnAddress, rest )

        _ ->
            Err "The topmost scope is not a local scope"


{-| Create a variable with local scope. Variables with local scope are visible
to the current function as well as to every function invoked by the current
function.

Overwrites any existing binding with `Undefined`.

-}
local : String -> List Scope -> List Scope
local name scopes =
    case scopes of
        (Local returnAddress { variables }) :: rest ->
            Local returnAddress { variables = (Dict.insert name Undefined variables) }
                :: rest

        _ ->
            scopes


{-| Create a new loop scope which is used to implement `repeat`.
-}
pushLoopScope : List Scope -> List Scope
pushLoopScope scopes =
    (Loop 0) :: scopes


{-| Remove the topmost scope if it is a loop scope.
-}
popLoopScope : List Scope -> Result String (List Scope)
popLoopScope scopes =
    case scopes of
        (Loop _) :: rest ->
            Ok rest

        _ ->
            Err "The topmost scope is not a loop scope"


{-| Increment the loop counter if the topmost scope is a loop scope.
-}
enterLoopScope : List Scope -> Result String (List Scope)
enterLoopScope scopes =
    case scopes of
        (Loop count) :: rest ->
            Ok <| Loop (count + 1) :: rest

        _ ->
            Err "The topmost scope is not a loop scope"


repcount : List Scope -> Int
repcount scopes =
    case scopes of
        (Loop count) :: rest ->
            count

        _ ->
            -1


{-| Create a new template scope which is used to implement `foreach`.
-}
pushTemplateScope : Type.Value -> List Scope -> List Scope
pushTemplateScope value scopes =
    (Template <| Iterator.initialize value) :: scopes


{-| Remove the topmost scope if it is a template scope.
-}
popTemplateScope : List Scope -> Result String (List Scope)
popTemplateScope scopes =
    case scopes of
        (Template _) :: rest ->
            Ok rest

        _ ->
            Err "The topmost scope is not a template scope"


{-| Advance the iterator if the topmost scope is a template scope.
-}
enterTemplateScope : List Scope -> Result String (List Scope)
enterTemplateScope scopes =
    case scopes of
        (Template iter) :: rest ->
            let
                newIter =
                    Iterator.step iter
            in
                Ok <| Template newIter :: rest

        _ ->
            Err "The topmost scope is not a loop scope"


{-| Get a template variable if the topmost scope is a template scope.
-}
templateVariable : Type.Value -> List Scope -> Result String Type.Value
templateVariable value scopes =
    case scopes of
        (Template iter) :: _ ->
            case value of
                Type.Word "rest" ->
                    Ok iter.rest

                _ ->
                    iter.current
                        |> Result.fromMaybe
                            ("? doesn't like "
                                ++ (Type.toString value)
                                ++ " as input"
                            )

        _ ->
            Err "The topmost scope is not a template scope"
