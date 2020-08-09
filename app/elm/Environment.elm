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
    )

{-| This module contains types and functions related to the state of a Logo
environment: the state of the turtle, console output etc.
-}

import Color exposing (Color)
import Environment.History exposing (Entry(..))
import Environment.Line as Line exposing (Line)
import Environment.Turtle as Turtle exposing (State(..), Turtle)


type Object
    = Line Line


type alias Environment =
    { history : List Entry
    , objects : List Object
    , turtle : Turtle
    , color : Color
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
    }


{-| Append an input to the console output.
-}
input : String -> Environment -> Environment
input string env =
    { env | history = Input string :: env.history }


{-| Append an error to the console output.
-}
error : String -> Environment -> Environment
error string env =
    { env | history = Error string :: env.history }


{-| Append a line to the console output.
-}
print : String -> Environment -> Environment
print string env =
    { env | history = Output string :: env.history }


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
        { env | turtle = newTurtle, objects = Line newLine :: env.objects }


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
        { env | turtle = newTurtle, objects = Line newLine :: env.objects }


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
