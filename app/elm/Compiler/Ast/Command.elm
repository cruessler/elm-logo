module Compiler.Ast.Command exposing
    ( Command(..)
    , all
    , arguments
    , find
    , name
    )

import Vm.Command as C


type Command
    = Command0 C.Command0
    | Command1 C.Command1
    | Command2 C.Command2


all : List Command
all =
    [ Command1 { name = "print", f = C.print }
    , Command1 { name = "type", f = C.type_ }
    , Command1 { name = "forward", f = C.forward }
    , Command1 { name = "back", f = C.back }
    , Command1 { name = "left", f = C.left }
    , Command1 { name = "right", f = C.right }
    , Command2 { name = "setxy", f = C.setxy }
    , Command0 { name = "pendown", f = C.pendown }
    , Command0 { name = "penup", f = C.penup }
    , Command1 { name = "setpencolor", f = C.setpencolor }
    , Command0 { name = "home", f = C.home }
    , Command0 { name = "clean", f = C.clean }
    , Command0 { name = "clearscreen", f = C.clearscreen }
    ]


find : String -> Maybe Command
find name_ =
    List.filter (\x -> name x == name_) all
        |> List.head


name : Command -> String
name command =
    case command of
        Command0 command0 ->
            command0.name

        Command1 command1 ->
            command1.name

        Command2 command2 ->
            command2.name


arguments : Command -> Int
arguments command =
    case command of
        Command0 _ ->
            0

        Command1 _ ->
            1

        Command2 _ ->
            2
