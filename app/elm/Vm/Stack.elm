module Vm.Stack exposing
    ( PrimitiveValue(..)
    , Stack
    , Value(..)
    , toTypeValue
    , toValue
    )

import Json.Encode as E
import Vm.Type as Type


{-| This module mainly holds the definition for a `Value` as it is represented
on the stack.

The introduction of arrays that are treated as references by UCBLogo poses a
few challenges as the stack did not have to deal with reference values so far.
This can be seen by the fact that there are now two different types,
`PrimitiveValue` and `Value`, used to represent values on the stack. At this
point, they are not clearly separated yet as `PrimitiveValue`, through its
variant `List`, can hold arrays, and thus reference values. This also leads to
certain programs’s behaviour diverging from their behaviour when run in
UCBLogo.

As of February 2024, I think a correct and simple solution would be to
integrate `ArrayId` into `PrimitiveValue` and convert `PrimitiveValue` to
`Type.Value` whenever a value is taken from the stack (and vice versa when it
is put onto the stack). Since this would come at a performance cost, I’m
slightly hesitant to adopt this solution.

-}
type PrimitiveValue
    = Word String
    | Int Int
    | Float Float
    | List (List Type.Value)


type Value
    = Void
    | Address Int
    | Value PrimitiveValue
    | ArrayId Int


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
            E.string <| Type.toDebugString <| toTypeValue value_

        ArrayId id ->
            E.string <| "array @" ++ String.fromInt id


toTypeValue : PrimitiveValue -> Type.Value
toTypeValue value =
    case value of
        Word string ->
            Type.Word string

        Int int ->
            Type.Int int

        Float float ->
            Type.Float float

        List list ->
            Type.List list


toValue : Stack -> E.Value
toValue =
    E.list encodeValue
