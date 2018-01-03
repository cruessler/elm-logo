module Vm.Type
    exposing
        ( Value(..)
        , toString
        , toInt
        )

{-| This module contains types and functions related to Logo types.
-}


{-| Represent a Logo value. A Logo value can currently be either a `Word` (i.
e. a string) or a `List`.

The variant `Int` is an optimization which is useful in two cases:

  - It can be used in places where numbers are expected, e.g. with `repeat`.
  - It enables integers to be put on the stack without the need to first
    convert them to a string which is useful to save return addresses.

For use with Logo primitives, an `Int` is considered to be a `Word` and will be
converted to its string representation as necessary.

-}
type Value
    = Word String
    | Int Int
    | List (List Value)


{-| Create a string representation of a `Value`.
-}
toString : Value -> String
toString value =
    case value of
        Word word ->
            word

        Int int ->
            Basics.toString int

        List list ->
            list
                |> List.map toString
                |> String.join " "


{-| Parse `Value` as an integer.
-}
toInt : Value -> Result String Int
toInt value =
    case value of
        Word word ->
            String.toInt word

        Int int ->
            Ok int

        _ ->
            Err <| "I don’t know how to convert " ++ (toString value) ++ " to an integer"
