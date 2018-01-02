module Vm.Iterator
    exposing
        ( Iterator
        , initialize
        , step
        )

{-| This module contains types and functions used in template-based iteration.
-}

import Vm.Type exposing (Value(..))


type alias Iterator =
    { current : Maybe Value
    , rest : Value
    }


initialize : Value -> Iterator
initialize value =
    { current = Nothing
    , rest = value
    }


step : Iterator -> Iterator
step iter =
    case iter.rest of
        Word "" ->
            { current = Nothing
            , rest = Word ""
            }

        Word word ->
            { current = Just <| Word <| String.left 1 word
            , rest = Word <| String.dropLeft 1 word
            }

        List (first :: rest) ->
            { current = Just first
            , rest = List rest
            }

        List [] ->
            { current = Nothing
            , rest = List []
            }
