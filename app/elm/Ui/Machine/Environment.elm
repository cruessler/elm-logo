module Ui.Machine.Environment exposing (Environment, Object(..), empty, environment, fromValue)

import Color exposing (Color)
import Environment.History exposing (Entry(..), History)
import Environment.Line as Line exposing (Line)
import Environment.Turtle as Turtle exposing (State(..), Turtle)
import Json.Decode as D
import Math.Vector2 as Vec2 exposing (Vec2)


type Object
    = Line Line


type alias Environment =
    { history : History
    , objects : List ( Int, Object )
    , turtle : Turtle
    }


empty : Environment
empty =
    { history = []
    , objects = []
    , turtle = Turtle.initialize
    }


decoderForEntry : String -> D.Decoder Entry
decoderForEntry type_ =
    case type_ of
        "Input" ->
            D.field "input" D.string |> D.map Input

        "Output" ->
            D.field "output" D.string |> D.map Output

        "PartialOutput" ->
            D.field "output" D.string |> D.map PartialOutput

        "Error" ->
            D.field "error" D.string |> D.map Error

        _ ->
            D.fail ("Invalid entry: " ++ type_)


decodeId : a -> D.Decoder ( Int, a )
decodeId value =
    D.field "id" D.int
        |> D.map (\id -> ( id, value ))


entry : D.Decoder ( Int, Entry )
entry =
    D.field "type" D.string
        |> D.andThen decoderForEntry
        |> D.andThen decodeId


history : D.Decoder History
history =
    D.list entry


vec2 : D.Decoder Vec2
vec2 =
    let
        toVec2 : Float -> Float -> Vec2
        toVec2 x y =
            Vec2.fromRecord { x = x, y = y }
    in
    D.map2 toVec2
        (D.field "x" D.float)
        (D.field "y" D.float)


color : D.Decoder Color
color =
    D.map4 Color.rgba
        (D.field "red" D.float)
        (D.field "green" D.float)
        (D.field "blue" D.float)
        (D.field "alpha" D.float)


decoderForObject : String -> D.Decoder Object
decoderForObject type_ =
    case type_ of
        "Line" ->
            D.map3 Line.Line
                (D.field "start" vec2)
                (D.field "end" vec2)
                (D.field "color" color)
                |> D.map Line

        _ ->
            D.fail ("Invalid object: " ++ type_)


object : D.Decoder ( Int, Object )
object =
    D.field "type" D.string
        |> D.andThen decoderForObject
        |> D.andThen decodeId


objects : D.Decoder (List ( Int, Object ))
objects =
    D.list object


state : D.Decoder State
state =
    D.string
        |> D.andThen
            (\state_ ->
                case state_ of
                    "Up" ->
                        D.succeed Up

                    "Down" ->
                        D.succeed Down

                    _ ->
                        D.fail ("Invalid object: " ++ state_)
            )


turtle : D.Decoder Turtle
turtle =
    D.map4 Turtle
        (D.field "x" D.float)
        (D.field "y" D.float)
        (D.field "direction" D.float)
        (D.field "state" state)


environment : D.Decoder Environment
environment =
    D.map3 Environment
        (D.field "history" history)
        (D.field "objects" objects)
        (D.field "turtle" turtle)


fromValue : D.Value -> Result D.Error Environment
fromValue =
    D.decodeValue environment
