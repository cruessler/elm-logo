module Vm.Types exposing (Value(..))

{-| This module contains types and functions related to Logo types.
-}


{-| Represent a Logo value. A Logo value can currently be either a `Word` (i. e. a string) or a `List`.
-}
type Value
    = Word String
    | List (List Value)


{-| Create a string representation of a `Value`.
-}
toString : Value -> String
toString value =
    case value of
        Word name ->
            "Word `" ++ name ++ "`"

        List list ->
            list
                |> List.map toString
                |> String.join ", "
