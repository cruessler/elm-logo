module Vm.Stack exposing
    ( Arrays
    , PrimitiveValue(..)
    , Stack
    , Value(..)
    , toValue
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Encode as E
import Vm.Type as Type


{-| This module mainly holds the definition for a `Value` as it is represented
on the stack.
-}
type PrimitiveValue
    = Word String
    | Int Int
    | Float Float
    | List (List PrimitiveValue)
    | ArrayId Int


type Value
    = Void
    | Address Int
    | Value PrimitiveValue


type alias Stack =
    List Value


type alias Arrays =
    Dict Int { items : Array PrimitiveValue, origin : Int, id : Maybe Int }


encodeValue : Arrays -> Value -> E.Value
encodeValue arrays value =
    case value of
        Void ->
            E.string "_"

        Address address ->
            E.string <| "@" ++ String.fromInt address

        Value primitive ->
            E.string <| Type.toDebugString <| toTypeValue arrays primitive


toTypeValue : Arrays -> PrimitiveValue -> Type.Value
toTypeValue arrays value =
    case value of
        Word string ->
            Type.Word string

        Int int ->
            Type.Int int

        Float float ->
            Type.Float float

        List list ->
            Type.List (List.map (toTypeValue arrays) list)

        ArrayId id ->
            Dict.get id arrays
                |> Maybe.map
                    (\array ->
                        let
                            { items, origin } =
                                array

                            typeItems =
                                Array.map (toTypeValue arrays) items
                        in
                        { items = typeItems, origin = origin, id = Just id }
                    )
                |> Maybe.map Type.Array
                |> Maybe.withDefault (Type.Word "<array not found>")


toValue : Arrays -> Stack -> E.Value
toValue arrays =
    E.list (encodeValue arrays)
