module Vm.Primitive
    exposing
        ( Primitive1
        , Primitive2
        , first
        , butfirst
        , count
        )

{-| This module contains types and functions related to Logo’s builtin functions.
-}

import Vm.Type exposing (Value(..))
import Dict exposing (Dict)


{-| Represent a builtin function that takes one argument.
-}
type alias Primitive1 =
    { name : String
    , f :
        Value -> Result String Value
    }


{-| Represent a builtin function that takes two arguments.
-}
type alias Primitive2 =
    { name : String
    , f :
        Value -> Value -> Result String Value
    }


{-| Get the first element of a `Value`.

    first (Word "word") == Ok (Word "w")
    first (List [ Word "word" ]) == Ok (Word "word")

Return `Err ...` for empty inputs.

-}
first : Value -> Result String Value
first value =
    case value of
        Word "" ->
            Err "first doesn’t like an empty word as input"

        Word str ->
            Ok <| Word <| String.left 1 str

        List (first :: rest) ->
            Ok first

        List [] ->
            Err "first doesn’t like [] as input"


{-| Get all but the first element of a `Value`.

    butfirst (Word "word") == Ok (Word "ord")
    butfirst (List [ Word "word" ]) == Ok (List [])

Return `Err ...` for empty inputs.

-}
butfirst : Value -> Result String Value
butfirst value =
    case value of
        Word "" ->
            Err "butfirst doesn’t like an empty word as input"

        Word str ->
            Ok <| Word <| String.dropLeft 1 str

        List (first :: rest) ->
            Ok <| List rest

        List [] ->
            Err "first doesn’t like [] as input"


{-| Count the number of characters in a `Word` or the number of elements in a
`List`.

    count (Word "word") == Ok (Word "4")
    count (List [ Word "word" ]) == Ok (Word "1")

-}
count : Value -> Result String Value
count value =
    let
        length =
            case value of
                Word word ->
                    String.length word

                List list ->
                    List.length list
    in
        length |> toString |> Word |> Ok
