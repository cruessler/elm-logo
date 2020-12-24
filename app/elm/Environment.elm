module Environment exposing
    ( Environment
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
    , setxy
    , toValue
    )

{-| This module contains types and functions related to the state of a Logo
environment: the state of the turtle, console output etc.
-}

import Color exposing (Color)
import Environment.History exposing (Entry(..), History)
import Environment.Line as Line exposing (Line)
import Environment.Turtle as Turtle exposing (State(..), Turtle)
import Json.Encode as E
import Math.Vector2 as Vec2 exposing (Vec2)


type Object
    = Line Line


type alias Environment =
    { history : History
    , objects : List ( Int, Object )
    , turtle : Turtle
    , color : Color
    , nextId : Int
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
    , nextId = 0
    }


encodeEntry : ( Int, Entry ) -> E.Value
encodeEntry ( id, entry ) =
    case entry of
        Input input_ ->
            E.object
                [ ( "type", E.string "Input" )
                , ( "id", E.int id )
                , ( "input", E.string input_ )
                ]

        Output output_ ->
            E.object
                [ ( "type", E.string "Output" )
                , ( "id", E.int id )
                , ( "output", E.string output_ )
                ]

        Error error_ ->
            E.object
                [ ( "type", E.string "Error" )
                , ( "id", E.int id )
                , ( "error", E.string error_ )
                ]


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
        [ ( "history", E.list encodeEntry history )
        , ( "objects", E.list encodeObject objects )
        , ( "turtle", encodeTurtle turtle )
        ]


pushHistoryEntry : Entry -> Environment -> Environment
pushHistoryEntry entry env =
    let
        nextId =
            env.nextId + 1
    in
    { env | nextId = nextId, history = ( env.nextId, entry ) :: env.history }


{-| Append an input to the console output.
-}
input : String -> Environment -> Environment
input string =
    pushHistoryEntry (Input string)


{-| Append an error to the console output.
-}
error : String -> Environment -> Environment
error string =
    pushHistoryEntry (Error string)


{-| Append a line to the console output.
-}
print : String -> Environment -> Environment
print string =
    pushHistoryEntry (Output string)


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
                Line.line env.turtle newTurtle env.color
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
                Line.line env.turtle newTurtle env.color
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
