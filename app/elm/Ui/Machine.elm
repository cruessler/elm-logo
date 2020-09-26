module Ui.Machine exposing (Machine, empty, fromValue, view)

import Array exposing (Array)
import Css
    exposing
        ( auto
        , backgroundColor
        , batch
        , boxShadow5
        , color
        , em
        , listStyle
        , margin
        , none
        , overflowY
        , padding
        , padding2
        , property
        , px
        , rgb
        , zero
        )
import Dict exposing (Dict)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Json.Decode as D
import Ui.Machine.Environment as Environment exposing (Environment)
import Ui.Machine.Scope as Scope exposing (Binding(..), Scope(..))
import Ui.Theme as Theme


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
    H.li
        [ A.css [ padding2 (em 0.2) zero ]
        ]
        [ H.text value ]


stack : List String -> Html msg
stack stack_ =
    let
        children =
            List.map stackValue stack_
    in
    H.div []
        [ H.h2 [ A.css [ margin zero ] ] [ H.text "Stack" ]
        , H.ul
            [ A.css
                [ Theme.monospaceFont
                , padding zero
                , listStyle none
                ]
            ]
            children
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
    H.table [ A.css [ Theme.monospaceFont ] ] children


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
                , H.table [ A.css [ Theme.monospaceFont ] ]
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
    H.div []
        [ H.h2 [ A.css [ margin zero ] ] [ H.text "Scopes" ]
        , H.ul
            [ A.css
                [ Theme.monospaceFont
                , padding zero
                , listStyle none
                ]
            ]
            children
        ]


instruction : Bool -> Int -> String -> Html msg
instruction current i instruction_ =
    let
        style =
            if current then
                batch
                    [ color (rgb 18 18 18)
                    , backgroundColor Theme.color
                    ]

            else
                batch []
    in
    H.tr
        [ A.css
            [ style ]
        ]
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
    H.div []
        [ H.h2 [ A.css [ margin zero ] ] [ H.text "Instructions" ]
        , H.h3 [] [ H.text "Next" ]
        , H.table [ A.css [ Theme.monospaceFont ] ] [ current ]
        , H.table
            [ A.css
                [ Theme.monospaceFont
                , overflowY auto
                ]
            ]
            children
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
    H.div
        [ A.css
            [ property "grid-area" "vm"
            , padding (em 1)
            , overflowY auto
            , color Theme.color
            , backgroundColor Theme.backgroundColor
            , boxShadow5 (px -5) (px 5) (px 5) zero (rgb 201 199 199)
            ]
        ]
        [ stack_, scopes_, instructions_ ]
