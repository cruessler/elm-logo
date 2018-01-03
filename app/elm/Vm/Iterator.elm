module Vm.Iterator
    exposing
        ( Iterator
        , initialize
        , step
        )

{-| This module contains types and functions used in template-based iteration.
-}

import Vm.Type as Type


type alias Iterator =
    { current : Maybe Type.Value
    , rest : Type.Value
    }


initialize : Type.Value -> Iterator
initialize value =
    { current = Nothing
    , rest = value
    }


step : Iterator -> Iterator
step iter =
    case iter.rest of
        Type.Word "" ->
            { current = Nothing
            , rest = Type.Word ""
            }

        Type.Word word ->
            { current = Just <| Type.Word <| String.left 1 word
            , rest = Type.Word <| String.dropLeft 1 word
            }

        Type.Int int ->
            let
                word =
                    toString int
            in
                { current = Just <| Type.Word <| String.left 1 word
                , rest = Type.Word <| String.dropLeft 1 word
                }

        Type.List (first :: rest) ->
            { current = Just first
            , rest = Type.List rest
            }

        Type.List [] ->
            { current = Nothing
            , rest = Type.List []
            }
