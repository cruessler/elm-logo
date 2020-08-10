port module Worker exposing (main)

import Json.Decode as D
import Json.Encode as E
import Logo exposing (Logo)
import Platform
import Vm.Vm as Vm
import Worker.Command as Command exposing (Command(..))


type alias Flags =
    ()


type alias Model =
    Logo


type Msg
    = Execute Command


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Logo.empty, Cmd.none )


executeCommand : Command -> Logo -> Logo
executeCommand command =
    case command of
        Compile source ->
            Logo.compile source

        Run source ->
            Logo.run source

        Continue ->
            Logo.continue

        Step ->
            Logo.step

        NoOp ->
            identity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Execute NoOp ->
            ( model, Cmd.none )

        Execute cmd ->
            let
                newLogo =
                    executeCommand cmd model

                vm =
                    Logo.getVm newLogo

                outCmd =
                    sendResult (Vm.toValue vm)
            in
            ( newLogo, outCmd )


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveCommand
        (Command.fromValue
            >> Result.withDefault NoOp
            >> Execute
        )


port receiveCommand : (D.Value -> msg) -> Sub msg


port sendResult : E.Value -> Cmd msg
