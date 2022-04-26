module Environment.Line exposing (Line, line)

import Color exposing (Color)
import Environment.Turtle exposing (Turtle)
import Math.Vector2 as Vec2 exposing (Vec2)


type alias Line =
    { start : Vec2
    , end : Vec2
    , width : Int
    , color : Color
    }


line : Turtle -> Turtle -> Int -> Color -> Line
line start end width color =
    { start = Vec2.vec2 start.x start.y
    , end = Vec2.vec2 end.x end.y
    , width = width
    , color = color
    }
