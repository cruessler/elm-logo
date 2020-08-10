module Ui.Machine.Scope exposing (Binding(..), Scope(..), fromValue, scope)

import Dict exposing (Dict)
import Json.Decode as D


type Binding
    = Undefined
    | Defined String


type Scope
    = Root (Dict String Binding)
    | Local Int (Dict String Binding)
    | Template (Maybe String) String
    | Loop Int


binding : D.Decoder Binding
binding =
    D.oneOf
        [ D.map Defined D.string
        , D.null Undefined
        ]


decoderForScope : String -> D.Decoder Scope
decoderForScope type_ =
    case type_ of
        "Root" ->
            D.map Root (D.field "variables" <| D.dict binding)

        "Local" ->
            D.map2 Local (D.field "address" D.int) (D.field "variables" <| D.dict binding)

        "Template" ->
            D.map2 Template (D.field "current" <| D.nullable D.string) (D.field "rest" D.string)

        "Loop" ->
            D.map Loop (D.field "current" D.int)

        _ ->
            D.fail ("Invalid type: " ++ type_)


scope : D.Decoder Scope
scope =
    D.field "type" D.string
        |> D.andThen decoderForScope


fromValue : D.Value -> Result D.Error Scope
fromValue =
    D.decodeValue scope
