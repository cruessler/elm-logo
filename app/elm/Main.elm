port module Main exposing (main)

import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Lazy exposing (lazy2)
import Json.Decode as D
import Json.Encode as E
import Task
import Ui.Canvas as Canvas exposing (Size)
import Ui.Examples as Examples
import Ui.Machine as Machine exposing (Machine)
import Ui.Terminal as Terminal
import Worker.Command as Command exposing (Command(..))


type alias Model =
    { windowSize : Maybe Size
    , currentCommandLine : String
    , machine : Machine
    }


type Msg
    = GetViewport Viewport
    | Resize Int Int
    | Input String
    | Compile
    | Step
    | Continue
    | Scroll
    | ReplaceMachine Machine


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { windowSize = Nothing
      , currentCommandLine = ""
      , machine = Machine.empty
      }
    , Task.perform GetViewport Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize Resize
        , receiveResult
            (Machine.fromValue
                >> Result.withDefault Machine.empty
                >> ReplaceMachine
            )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize width height ->
            let
                windowSize =
                    Just { width = width, height = height }
            in
            ( { model | windowSize = windowSize }, Cmd.none )

        GetViewport info ->
            let
                windowSize =
                    Just
                        { width = round info.scene.width
                        , height = round info.scene.height
                        }
            in
            ( { model
                | windowSize = windowSize
              }
            , Cmd.none
            )

        Input newCommandLine ->
            ( { model | currentCommandLine = newCommandLine }, Cmd.none )

        Compile ->
            let
                command =
                    Command.Compile model.currentCommandLine

                outCmd =
                    sendCommand (Command.toValue command)
            in
            ( model, outCmd )

        Step ->
            let
                outCmd =
                    sendCommand (Command.toValue Command.Step)
            in
            ( model, outCmd )

        Continue ->
            let
                id =
                    "history"

                scrollCmd =
                    Dom.getViewportOf id
                        |> Task.andThen (\info -> Dom.setViewportOf id 0 info.scene.height)
                        |> Task.attempt (\_ -> Scroll)

                outCmd =
                    sendCommand (Command.toValue Command.Continue)
            in
            ( model, Cmd.batch [ outCmd, scrollCmd ] )

        Scroll ->
            ( model, Cmd.none )

        ReplaceMachine newMachine ->
            ( { model | machine = newMachine }, Cmd.none )


overlayLeft : Model -> Html Msg
overlayLeft model =
    let
        config =
            { onInput = Input
            , onCompile = Compile
            , onStep = Step
            , onContinue = Continue
            }
    in
    H.div [ A.id "overlay-left" ]
        [ Terminal.view
            config
            { currentText = model.currentCommandLine
            , history = model.machine.environment.history
            }
        , Examples.view { onClick = Input }
        ]


overlay : Model -> Html Msg
overlay model =
    H.div [ A.id "overlay" ]
        [ overlayLeft model
        , Machine.view model.machine
        ]


view : Model -> Html Msg
view model =
    let
        canvas =
            model.windowSize
                |> Maybe.map
                    (\size ->
                        lazy2 Canvas.view size model.machine.environment
                    )
                |> Maybe.withDefault (H.text "")

        children =
            [ canvas
            , overlay model
            ]
    in
    H.div [ A.id "main" ] children


port sendCommand : E.Value -> Cmd msg


port receiveResult : (D.Value -> msg) -> Sub msg
