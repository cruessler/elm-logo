module Vm.Type exposing
    ( Error
    , Value(..)
    , false
    , fromBool
    , fromFloat
    , toBool
    , toDebugString
    , toFloat
    , toInt
    , toString
    , true
    )

{-| This module contains types and functions related to Logo types.
-}


{-| Represent a Logo value. A Logo value can currently be either a `Word` (i.
e. a string) or a `List`.

The variant `Int` is an optimization which is useful in two cases:

  - It can be used in places where integers are expected, e. g. with `repeat`.
  - It enables integers to be put on the stack without the need to first
    convert them to a string which is useful to save return addresses.

The variant `Float` is an optimization which is useful when dealing with
numbers which can be parsed once at compile time. It also enables faster type
checking for Logo primitives which expect floats as input, e. g. `sum`.

For use with Logo primitives, `Int` as well as `Float` are considered to be a
`Word` and will be converted to their string representation as necessary.

-}
type Value
    = Word String
    | Int Int
    | Float Float
    | List (List Value)


type Error
    = NoInt String
    | NoFloat String
    | NoBool String


{-| Create a string representation of a `Value`.
-}
toString : Value -> String
toString value =
    let
        inList v =
            case v of
                List list ->
                    let
                        string =
                            list
                                |> List.map inList
                                |> String.join " "
                    in
                    "[" ++ string ++ "]"

                _ ->
                    toString v
    in
    case value of
        Word word ->
            word

        Int int ->
            String.fromInt int

        Float float ->
            String.fromFloat float

        List list ->
            list
                |> List.map inList
                |> String.join " "


{-| Create a string representation of a `Value` for use in debug messages.

    butfirst [ 1 ] ; You don’t say what to do with []
    butfirst "a ; You don’t say what to do with ||

-}
toDebugString : Value -> String
toDebugString value =
    case value of
        Word "" ->
            "||"

        List [] ->
            "[]"

        List _ ->
            "[" ++ toString value ++ "]"

        _ ->
            toString value


{-| Parse `Value` as an integer.
-}
toInt : Value -> Result Error Int
toInt value =
    case value of
        Word word ->
            String.toInt word |> Result.fromMaybe (NoInt word)

        Int int ->
            Ok int

        Float float ->
            let
                maybeInt =
                    round float
            in
            if (round float |> Basics.toFloat) == float then
                Ok maybeInt

            else
                Err <| NoInt (String.fromFloat float)

        _ ->
            Err <| NoInt (toString value)


{-| Parse `Value` as a float.
-}
toFloat : Value -> Result Error Float
toFloat value =
    case value of
        Word word ->
            String.toFloat word |> Result.fromMaybe (NoFloat word)

        Int int ->
            Ok (Basics.toFloat int)

        Float float ->
            Ok float

        _ ->
            Err <| NoFloat (toString value)


{-| Parse `Value` as a bool.
-}
toBool : Value -> Result Error Bool
toBool value =
    case value of
        Word word ->
            case String.toLower word of
                "true" ->
                    Ok True

                "false" ->
                    Ok False

                _ ->
                    Err <| NoBool word

        _ ->
            Err <| NoBool (toString value)


true : Value
true =
    Word "true"


false : Value
false =
    Word "false"


{-| Convert a float to a `Value`.
-}
fromFloat : Float -> Value
fromFloat =
    Float


{-| Convert a bool to a `Value`.
-}
fromBool : Bool -> Value
fromBool bool =
    if bool then
        true

    else
        false
