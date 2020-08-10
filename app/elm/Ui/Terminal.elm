module Ui.Terminal exposing (view)

import Environment.History exposing (Entry(..))
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E


type alias Config msg =
    { onInput : String -> msg
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
    case entry of
        Input command ->
            H.li [ A.class "input" ] [ H.text command ]

        Output string ->
            H.li [ A.class "output" ] [ H.text string ]

        Error message ->
            H.li [ A.class "error" ] [ H.text message ]


prompt : Config msg -> Model -> Html msg
prompt { onInput } { currentText } =
    H.textarea
        [ A.id "command-line"
        , A.value currentText
        , A.placeholder help
        , E.onInput onInput
        ]
        []


history : Config msg -> Model -> Html msg
history config model =
    let
        entries =
            List.map line model.history |> List.reverse
    in
    H.div [ A.id "history" ]
        [ H.ul [ A.id "entries" ] entries
        , prompt config model
        ]


help : String
help =
    """This is a prototype of elm-logo, a Logo interpreter written in Elm"""


controls : Config msg -> Html msg
controls { onCompile, onContinue, onStep } =
    H.div [ A.id "controls" ]
        [ H.button
            [ A.id "compile"
            , E.onClick onCompile
            ]
            [ H.text "Compile" ]
        , H.button
            [ A.id "step"
            , E.onClick onStep
            ]
            [ H.text "Step" ]
        , H.button
            [ A.id "run"
            , E.onClick onContinue
            ]
            [ H.text "Continue" ]
        ]


view : Config msg -> Model -> Html msg
view config model =
    H.div [ A.id "terminal" ]
        [ history config model
        , controls config
        ]
