module Vm.Primitive
    exposing
        ( Primitive1
        , Primitive2
        , first
        , butfirst
        , count
        , lessThan
        , emptyp
        )

{-| This module contains types and functions related to Logo’s builtin functions.
-}

import Vm.Type as Type
import Dict exposing (Dict)


{-| Represent a builtin function that takes one argument.
-}
type alias Primitive1 =
    { name : String
    , f :
        Type.Value -> Result String Type.Value
    }


{-| Represent a builtin function that takes two arguments.
-}
type alias Primitive2 =
    { name : String
    , f :
        Type.Value -> Type.Value -> Result String Type.Value
    }


{-| Get the first element of a `Value`.

    first (Word "word") == Ok (Word "w")
    first (List [ Word "word" ]) == Ok (Word "word")

Return `Err ...` for empty inputs.

-}
first : Type.Value -> Result String Type.Value
first value =
    case value of
        Type.Word "" ->
            Err "first doesn’t like an empty word as input"

        Type.Word str ->
            Ok <| Type.Word <| String.left 1 str

        Type.List (first :: rest) ->
            Ok first

        Type.List [] ->
            Err "first doesn’t like [] as input"


{-| Get all but the first element of a `Value`.

    butfirst (Word "word") == Ok (Word "ord")
    butfirst (List [ Word "word" ]) == Ok (List [])

Return `Err ...` for empty inputs.

-}
butfirst : Type.Value -> Result String Type.Value
butfirst value =
    case value of
        Type.Word "" ->
            Err "butfirst doesn’t like an empty word as input"

        Type.Word str ->
            Ok <| Type.Word <| String.dropLeft 1 str

        Type.List (first :: rest) ->
            Ok <| Type.List rest

        Type.List [] ->
            Err "first doesn’t like [] as input"


{-| Count the number of characters in a `Word` or the number of elements in a
`List`.

    count (Word "word") == Ok (Word "4")
    count (List [ Word "word" ]) == Ok (Word "1")

-}
count : Type.Value -> Result String Type.Value
count value =
    let
        length =
            case value of
                Type.Word word ->
                    String.length word

                Type.List list ->
                    List.length list
    in
        length |> toString |> Type.Word |> Ok


{-| Convert two values to numbers and compare whether the first one is less
than the second one.

    lessThan (Word "0") (Word "1") == Ok (Word "true")
    lessThan (Word "1") (Word "1") == Ok (Word "false")
    lessThan (Word "a") (Word "1") == Err _

-}
lessThan : Type.Value -> Type.Value -> Result String Type.Value
lessThan value1 value2 =
    Type.toInt value1
        |> Result.andThen
            (\int1 ->
                Type.toInt value2
                    |> Result.andThen
                        (\int2 ->
                            if int1 < int2 then
                                Ok (Type.Word "true")
                            else
                                Ok (Type.Word "false")
                        )
            )


{-| Check whether a given `Value` is empty. Only the empty `Word` and the empty
`List` are considered empty.

    emptyp (Word "") == Word "true"
    emptyp (Word "word") == Word "false"

-}
emptyp : Type.Value -> Result String Type.Value
emptyp value =
    case value of
        Type.Word "" ->
            Ok (Type.Word "true")

        Type.List [] ->
            Ok (Type.Word "true")

        _ ->
            Ok (Type.Word "false")
