module Environment exposing
    ( Arrays
    , Environment
    , Object(..)
    , back
    , clean
    , empty
    , error
    , forward
    , home
    , input
    , left
    , pendown
    , penup
    , print
    , right
    , setpencolor
    , setpensize
    , setxy
    , toValue
    , type_
    )

{-| This module contains types and functions related to the state of a Logo
environment: the state of the turtle, console output etc.
-}

import Array exposing (Array)
import Color exposing (Color)
import Dict exposing (Dict)
import Environment.History as History exposing (Entry(..), History)
import Environment.Line as Line exposing (Line)
import Environment.Turtle as Turtle exposing (State(..), Turtle)
import Json.Encode as E
import Math.Vector2 as Vec2 exposing (Vec2)
import Vm.Type as Type


type Object
    = Line Line


type alias Arrays =
    Dict Int { items : Array Type.Value, origin : Int, id : Maybe Int }


type alias Environment =
    { history : History
    , objects : List ( Int, Object )
    , turtle : Turtle
    , penSize : Int
    , color : Color
    , nextId : Int
    , arrays : Arrays
    , nextArrayId : Int
    }


defaultColor : Color
defaultColor =
    Color.black


empty : Environment
empty =
    { history = []
    , objects = []
    , turtle = Turtle.initialize
    , color = defaultColor
    , penSize = 1
    , nextId = 0
    , arrays = Dict.empty
    , nextArrayId = 0
    }


encodeVec2 : Vec2 -> E.Value
encodeVec2 vec2 =
    let
        { x, y } =
            Vec2.toRecord vec2
    in
    E.object [ ( "x", E.float x ), ( "y", E.float y ) ]


encodeColor : Color -> E.Value
encodeColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    E.object
        [ ( "red", E.float red )
        , ( "green", E.float green )
        , ( "blue", E.float blue )
        , ( "alpha", E.float alpha )
        ]


encodeLine : Int -> Line -> E.Value
encodeLine id line =
    E.object
        [ ( "type", E.string "Line" )
        , ( "id", E.int id )
        , ( "start", encodeVec2 line.start )
        , ( "end", encodeVec2 line.end )
        , ( "width", E.int line.width )
        , ( "color", encodeColor line.color )
        ]


encodeObject : ( Int, Object ) -> E.Value
encodeObject ( id, object ) =
    case object of
        Line line ->
            encodeLine id line


encodeState : State -> E.Value
encodeState state =
    case state of
        Down ->
            E.string "Down"

        Up ->
            E.string "Up"


encodeTurtle : Turtle -> E.Value
encodeTurtle { x, y, direction, state } =
    E.object
        [ ( "x", E.float x )
        , ( "y", E.float y )
        , ( "direction", E.float direction )
        , ( "state", encodeState state )
        ]


toValue : Environment -> E.Value
toValue { history, objects, turtle } =
    E.object
        [ ( "history", History.toValue history )
        , ( "objects", E.list encodeObject objects )
        , ( "turtle", encodeTurtle turtle )
        ]


{-| Append an input to the console output.

If the last entry is a partial line as produced by `type`, make the input the
second to last entry so that the partial line is still the last line.

-}
input : String -> Environment -> Environment
input string env =
    let
        nextId =
            env.nextId + 1

        inputWithId =
            ( env.nextId, Input string )
    in
    case env.history of
        (( _, PartialOutput _ ) as partialOutput) :: rest ->
            { env
                | nextId = nextId
                , history = partialOutput :: inputWithId :: rest
            }

        _ ->
            { env
                | nextId = nextId
                , history = inputWithId :: env.history
            }


{-| Append an error to the console output.

If the last entry is a partial line as produced by `type`, mark it as complete
before appending the new entry.

-}
error : String -> Environment -> Environment
error string env =
    let
        nextId =
            env.nextId + 1

        errorWithId =
            ( env.nextId, Error string )
    in
    case env.history of
        ( id, PartialOutput output_ ) :: rest ->
            { env
                | nextId = nextId
                , history = errorWithId :: ( id, Output output_ ) :: rest
            }

        _ ->
            { env
                | nextId = nextId
                , history = errorWithId :: env.history
            }


appendCharacter : Char -> Environment -> Environment
appendCharacter char env =
    case env.history of
        ( id, PartialOutput output_ ) :: rest ->
            -- '\u{000D}' is '\r'.
            -- This doesn’t properly handle "\r\n".
            if char == '\n' || char == '\u{000D}' then
                { env | history = ( id, Output output_ ) :: rest }

            else
                { env | history = ( id, PartialOutput (output_ ++ String.fromChar char) ) :: rest }

        _ ->
            let
                nextId =
                    env.nextId + 1
            in
            -- '\u{000D}' is '\r'.
            -- This doesn’t properly handle "\r\n".
            if char == '\n' || char == '\u{000D}' then
                { env
                    | nextId = nextId
                    , history = ( env.nextId, Output "" ) :: env.history
                }

            else
                { env
                    | nextId = nextId
                    , history = ( env.nextId, PartialOutput (String.fromChar char) ) :: env.history
                }


{-| Append a line to the console output.
-}
print : String -> Environment -> Environment
print string env =
    -- Since `appendCharacter` doesn’t properly handle "\r\n", we replace it by
    -- "\n". This is not optimal, but sufficient for the time being.
    string
        |> String.replace "\u{000D}\n" "\n"
        |> String.foldl appendCharacter env
        |> appendCharacter '\n'


{-| Print characters to the console output, but don’t append a newline or space
at the end.

A subsequent invocation of `type_` will write its argument to the same line. A
subsequent invocation of `print` will write its argument to the same line and
add a newline.

-}
type_ : String -> Environment -> Environment
type_ string env =
    -- Since `appendCharacter` doesn’t properly handle "\r\n", we replace it by
    -- "\n". This is not optimal, but sufficient for the time being.
    string
        |> String.replace "\u{000D}\n" "\n"
        |> String.foldl appendCharacter env


pushObject : Object -> Environment -> Environment
pushObject object env =
    let
        nextId =
            env.nextId + 1
    in
    { env | nextId = nextId, objects = ( env.nextId, object ) :: env.objects }


{-| Move the turtle forward.

Draw a line if the turtle is down.

-}
forward : Float -> Environment -> Environment
forward by env =
    if env.turtle.state == Turtle.Up then
        { env | turtle = Turtle.forward by env.turtle }

    else
        let
            newTurtle =
                Turtle.forward by env.turtle

            newLine =
                Line.line env.turtle newTurtle env.penSize env.color
        in
        { env | turtle = newTurtle }
            |> pushObject (Line newLine)


{-| Move the turtle back.

Draw a line if the turtle is down.

-}
back : Float -> Environment -> Environment
back by env =
    if env.turtle.state == Turtle.Up then
        { env | turtle = Turtle.back by env.turtle }

    else
        let
            newTurtle =
                Turtle.back by env.turtle

            newLine =
                Line.line env.turtle newTurtle env.penSize env.color
        in
        { env | turtle = newTurtle }
            |> pushObject (Line newLine)


{-| Turn the turtle left.
-}
left : Float -> Environment -> Environment
left by env =
    { env | turtle = Turtle.left by env.turtle }


{-| Turn the turtle right.
-}
right : Float -> Environment -> Environment
right by env =
    { env | turtle = Turtle.right by env.turtle }


{-| Lift the pen.

In this state, turtle movements will not lead to objects being drawn.

-}
penup : Environment -> Environment
penup env =
    { env | turtle = Turtle.penup env.turtle }


{-| Put the pen down.

In this state, turtle movements will lead to objects being drawn.

-}
pendown : Environment -> Environment
pendown env =
    { env | turtle = Turtle.pendown env.turtle }


{-| Set the pen’s color.

This affects objects that are drawn by turtle movements.

-}
setpencolor : Int -> Environment -> Environment
setpencolor color_ env =
    let
        newColor =
            case remainderBy 16 color_ of
                0 ->
                    Color.black

                1 ->
                    Color.blue

                2 ->
                    Color.green

                3 ->
                    Color.lightBlue

                4 ->
                    Color.red

                5 ->
                    Color.darkRed

                6 ->
                    Color.yellow

                7 ->
                    Color.white

                8 ->
                    Color.brown

                9 ->
                    Color.lightBrown

                10 ->
                    Color.lightGreen

                11 ->
                    Color.darkBlue

                12 ->
                    Color.lightRed

                13 ->
                    Color.purple

                14 ->
                    Color.orange

                15 ->
                    Color.grey

                _ ->
                    defaultColor
    in
    { env | color = newColor }


{-| Set the pen’s size.

This affects objects that are drawn by turtle movements.

-}
setpensize : Int -> Environment -> Environment
setpensize penSize env =
    { env | penSize = penSize }


{-| Move the turtle to a specified position.

Regardless of the turtle’s state, this does not draw objects.

-}
setxy : Float -> Float -> Environment -> Environment
setxy x y env =
    { env | turtle = Turtle.setxy x y env.turtle }


{-| Move the turtle to its initial position.

Regardless of the turtle’s state, this does not draw objects.

-}
home : Environment -> Environment
home env =
    { env | turtle = Turtle.home env.turtle }


{-| Remove all objects that have been drawn so far.
-}
clean : Environment -> Environment
clean env =
    { env | objects = [] }
