module Ui.Machine exposing (Machine, empty, fromValue, view)

import Array exposing (Array)
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as A
import Json.Decode as D
import Ui.Machine.Environment as Environment exposing (Environment)
import Ui.Machine.Scope as Scope exposing (Binding(..), Scope(..))


type alias Machine =
    { instructions : Array String
    , programCounter : Int
    , stack : List String
    , scopes : List Scope
    , environment : Environment
    }


empty : Machine
empty =
    Machine (Array.fromList []) 0 [] [] Environment.empty


machine : D.Decoder Machine
machine =
    D.map5 Machine
        (D.field "instructions" <| D.array D.string)
        (D.field "programCounter" D.int)
        (D.field "stack" <| D.list D.string)
        (D.field "scopes" <| D.list Scope.scope)
        (D.field "environment" Environment.environment)


fromValue : D.Value -> Result D.Error Machine
fromValue =
    D.decodeValue machine


stackValue : String -> Html msg
stackValue value =
    H.li [] [ H.text value ]


stack : List String -> Html msg
stack stack_ =
    let
        children =
            List.map stackValue stack_
    in
    H.div [ A.id "stack" ]
        [ H.h2 [] [ H.text "Stack" ]
        , H.ul [] children
        ]


binding : Binding -> String
binding binding_ =
    case binding_ of
        Undefined ->
            "_"

        Defined value ->
            value


variable : String -> Binding -> Html msg
variable name binding_ =
    H.tr []
        [ H.td [] [ H.text name ]
        , H.td [] [ H.text <| binding binding_ ]
        ]


variables : Dict String Binding -> Html msg
variables variables_ =
    let
        children =
            Dict.map variable variables_
                |> Dict.values
    in
    H.table [] children


scope : Scope -> Html msg
scope scope_ =
    case scope_ of
        Root variables_ ->
            H.li [] [ H.h3 [] [ H.text "Root" ], variables variables_ ]

        Local address variables_ ->
            H.li []
                [ H.h3 []
                    [ H.text <| "Local@" ++ String.fromInt address
                    ]
                , variables variables_
                ]

        Template current rest ->
            H.li []
                [ H.h3 [] [ H.text "Template" ]
                , H.table []
                    [ H.tr []
                        [ H.td [] [ H.text "current" ]
                        , H.td [] [ H.text <| Maybe.withDefault "_" current ]
                        ]
                    , H.tr []
                        [ H.td [] [ H.text "rest" ]
                        , H.td [] [ H.text rest ]
                        ]
                    ]
                ]

        Loop i ->
            H.li [] [ H.h3 [] [ H.text <| "Loop@" ++ String.fromInt i ] ]


scopes : List Scope -> Html msg
scopes scopes_ =
    let
        children =
            List.map scope scopes_
    in
    H.div [ A.id "scopes" ]
        [ H.h2 [] [ H.text "Scopes" ]
        , H.ul [] children
        ]


instruction : Bool -> Int -> String -> Html msg
instruction current i instruction_ =
    H.tr [ A.classList [ ( "current", current ) ] ]
        [ H.td [] [ H.text <| String.fromInt i ]
        , H.td [] [ H.text <| instruction_ ]
        ]


instructions : Int -> Array String -> Html msg
instructions programCounter instructions_ =
    let
        current =
            instructions_
                |> Array.get programCounter
                |> Maybe.map (instruction True programCounter)
                |> Maybe.withDefault (H.tr [] [ H.td [] [ H.text "none" ] ])

        children =
            Array.indexedMap
                (\i instruction_ -> instruction (i == programCounter) i instruction_)
                instructions_
                |> Array.toList
    in
    H.div [ A.id "instructions" ]
        [ H.h2 [] [ H.text "Instructions" ]
        , H.h3 [] [ H.text "Next" ]
        , H.table [] [ current ]
        , H.table [ A.class "instructions" ] children
        ]


view : Machine -> Html msg
view machine_ =
    let
        stack_ =
            stack machine_.stack

        scopes_ =
            scopes machine_.scopes

        instructions_ =
            instructions machine_.programCounter machine_.instructions
    in
    H.div [ A.id "vm" ] [ stack_, scopes_, instructions_ ]
