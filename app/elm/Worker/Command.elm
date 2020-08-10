module Worker.Command exposing (Command(..), fromValue, toValue)

import Json.Decode as D
import Json.Encode as E


type Command
    = Compile String
    | Run String
    | Continue
    | Step
    | NoOp


compileCommand : D.Decoder Command
compileCommand =
    D.field "source" D.string |> D.map Compile


runCommand : D.Decoder Command
runCommand =
    D.field "source" D.string |> D.map Run


decoderForType : String -> D.Decoder Command
decoderForType type_ =
    case type_ of
        "Compile" ->
            compileCommand

        "Run" ->
            runCommand

        "Continue" ->
            D.succeed Continue

        "Step" ->
            D.succeed Step

        "NoOp" ->
            D.succeed NoOp

        _ ->
            D.fail ("Invalid command: " ++ type_)


command : D.Decoder Command
command =
    D.field "type" D.string
        |> D.andThen decoderForType


fromValue : D.Value -> Result D.Error Command
fromValue =
    D.decodeValue command


toValue : Command -> E.Value
toValue cmd =
    case cmd of
        Compile source ->
            E.object [ ( "type", E.string "Compile" ), ( "source", E.string source ) ]

        Run source ->
            E.object [ ( "type", E.string "Run" ), ( "source", E.string source ) ]

        Continue ->
            E.object [ ( "type", E.string "Continue" ) ]

        Step ->
            E.object [ ( "type", E.string "Step" ) ]

        NoOp ->
            E.object [ ( "type", E.string "NoOp" ) ]
