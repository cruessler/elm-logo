module Vm.Stack exposing
    ( Stack
    , Value(..)
    , toValue
    )

import Json.Encode as E
import Vm.Type as Type


type Value
    = Void
    | Address Int
    | Value Type.Value


type alias Stack =
    List Value


encodeValue : Value -> E.Value
encodeValue value =
    case value of
        Void ->
            E.string "_"

        Address address ->
            E.string <| "@" ++ String.fromInt address

        Value value_ ->
            E.string <| Type.toDebugString value_


toValue : Stack -> E.Value
toValue =
    E.list encodeValue
