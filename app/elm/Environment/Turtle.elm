module Environment.Turtle exposing
    ( State(..)
    , Turtle
    , back
    , forward
    , home
    , initialize
    , left
    , pendown
    , penup
    , right
    , setxy
    )


type State
    = Up
    | Down


type alias Turtle =
    { x : Float
    , y : Float
    , direction : Float
    , state : State
    }


initialize : Turtle
initialize =
    { x = 0, y = 0, direction = 0, state = Up }


forward : Float -> Turtle -> Turtle
forward by ({ x, y, direction } as turtle) =
    let
        newX =
            x + sin direction * by

        newY =
            y - cos direction * by
    in
    { turtle
        | x = newX
        , y = newY
        , direction = direction
    }


back : Float -> Turtle -> Turtle
back by ({ x, y, direction } as turtle) =
    let
        newX =
            x - sin direction * by

        newY =
            y + cos direction * by
    in
    { turtle
        | x = newX
        , y = newY
        , direction = direction
    }


left : Float -> Turtle -> Turtle
left by ({ direction } as turtle) =
    let
        newDirection =
            direction - degrees by
    in
    { turtle
        | direction = newDirection
    }


right : Float -> Turtle -> Turtle
right by ({ direction } as turtle) =
    let
        newDirection =
            direction + degrees by
    in
    { turtle
        | direction = newDirection
    }


setxy : Float -> Float -> Turtle -> Turtle
setxy x y turtle =
    { turtle | x = x, y = y }


penup : Turtle -> Turtle
penup turtle =
    { turtle | state = Up }


pendown : Turtle -> Turtle
pendown turtle =
    { turtle | state = Down }


home : Turtle -> Turtle
home turtle =
    { turtle | x = 0, y = 0, direction = 0 }
