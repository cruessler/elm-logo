module Ui.Terminal exposing (view)

import Css
    exposing
        ( auto
        , backgroundColor
        , before
        , borderStyle
        , color
        , displayFlex
        , hex
        , margin
        , marginLeft
        , none
        , overflowY
        , padding
        , pct
        , preWrap
        , property
        , px
        , rgb
        , rgba
        , whiteSpace
        , width
        , zero
        )
import Environment.History exposing (Entry(..))
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as D
import Ui.Theme as Theme


type alias Config msg =
    { onInput : String -> msg
    , onRun : msg
    , onCompile : msg
    , onContinue : msg
    , onStep : msg
    }


type alias Model =
    { currentText : String
    , history : List Entry
    }


line : Entry -> Html msg
line entry =
    let
        style : Css.Style
        style =
            whiteSpace preWrap
    in
    case entry of
        Input command ->
            H.li
                [ A.css
                    [ style
                    , before [ property "content" "\"> \"" ]
                    , color (rgba 247 248 242 0.7)
                    ]
                ]
                [ H.text command ]

        Output string ->
            H.li [ A.css [ style ] ] [ H.text string ]

        Error message ->
            H.li
                [ A.css
                    [ style
                    , before [ property "content" "\"! \"" ]
                    , color (rgb 248 80 80)
                    ]
                ]
                [ H.text message ]


onEnter : msg -> H.Attribute msg
onEnter onRun =
    let
        decoder =
            D.map2
                (\key ctrlKey ->
                    case ( key, ctrlKey ) of
                        ( "Enter", True ) ->
                            Just onRun

                        _ ->
                            Nothing
                )
                (D.field "key" D.string)
                (D.field "ctrlKey" D.bool)
                |> D.andThen (Maybe.map D.succeed >> Maybe.withDefault (D.fail ""))
    in
    E.on "keypress" decoder


prompt : Config msg -> Model -> Html msg
prompt { onInput, onRun } { currentText } =
    H.textarea
        [ A.css
            [ property "grid-area" "prompt"
            , width (pct 100)
            , Theme.monospaceFont
            , backgroundColor (hex "312f2f")
            , borderStyle none
            , color (hex "eaeaf0")
            ]
        , A.value currentText
        , A.placeholder help
        , E.onInput onInput
        , onEnter onRun
        ]
        []


history : Config msg -> Model -> Html msg
history config model =
    let
        entries =
            List.map line model.history |> List.reverse
    in
    H.div
        [ A.css
            [ property "display" "grid"
            , property "grid-template-rows" "5fr 1fr"
            , property "grid-template-areas" "\"entries\" \"prompt\""
            , overflowY auto
            ]
        ]
        [ H.ul
            [ A.css
                [ property "grid-area" "entries"
                , margin zero
                , padding zero
                , Theme.monospaceFont
                , overflowY auto
                ]
            ]
            entries
        , prompt config model
        ]


help : String
help =
    """This is a prototype of elm-logo, a Logo interpreter written in Elm"""


controls : Config msg -> Html msg
controls { onCompile, onContinue, onStep } =
    let
        buttonStyle : Css.Style
        buttonStyle =
            marginLeft (px 10)
    in
    H.div
        [ A.css
            [ property "grid-area" "controls"
            , padding (px 10)
            , displayFlex
            ]
        ]
        [ H.button
            [ A.css [ Theme.buttonStyle, marginLeft auto ]
            , E.onClick onCompile
            ]
            [ H.text "Compile" ]
        , H.button
            [ A.css [ Theme.buttonStyle, buttonStyle ]
            , E.onClick onStep
            ]
            [ H.text "Step" ]
        , H.button
            [ A.css [ Theme.buttonStyle, buttonStyle ]
            , E.onClick onContinue
            ]
            [ H.text "Continue" ]
        ]


view : Config msg -> Model -> Html msg
view config model =
    H.div
        [ A.css
            [ property "grid-area" "terminal"
            , property "display" "grid"
            , property "grid-template-rows" "1fr auto"
            , property "grid-template-areas" "\"history\" \"controls\""
            , overflowY auto
            , color Theme.color
            , backgroundColor Theme.backgroundColor
            ]
        ]
        [ history config model
        , controls config
        ]
