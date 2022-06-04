module Compiler.Ast.Command exposing
    ( Command(..)
    , all
    , find
    , name
    , numberOfDefaultArguments
    )

import Vm.Command as C


type Command
    = Command0 C.Command0
    | Command1 C.Command1
    | Command2 C.Command2
    | CommandN C.CommandN


all : List Command
all =
    [ CommandN { name = "print", f = C.printN, numberOfDefaultArguments = 1 }
    , CommandN { name = "pr", f = C.printN, numberOfDefaultArguments = 1 }
    , Command1 { name = "show", f = C.show }
    , Command1 { name = "type", f = C.type_ }
    , Command1 { name = "forward", f = C.forward }
    , Command1 { name = "fd", f = C.forward }
    , Command1 { name = "back", f = C.back }
    , Command1 { name = "bk", f = C.back }
    , Command1 { name = "left", f = C.left }
    , Command1 { name = "lt", f = C.left }
    , Command1 { name = "right", f = C.right }
    , Command1 { name = "rt", f = C.right }
    , Command2 { name = "setxy", f = C.setxy }
    , Command0 { name = "pendown", f = C.pendown }
    , Command0 { name = "pd", f = C.pendown }
    , Command0 { name = "penup", f = C.penup }
    , Command0 { name = "pu", f = C.penup }
    , Command1 { name = "setpencolor", f = C.setpencolor }
    , Command1 { name = "setpc", f = C.setpencolor }
    , Command1 { name = "setpensize", f = C.setpensize }
    , Command0 { name = "home", f = C.home }
    , Command0 { name = "clean", f = C.clean }
    , Command0 { name = "clearscreen", f = C.clearscreen }
    , Command0 { name = "cs", f = C.clearscreen }
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

        CommandN commandN ->
            commandN.name


numberOfDefaultArguments : Command -> Int
numberOfDefaultArguments command =
    case command of
        Command0 _ ->
            0

        Command1 _ ->
            1

        Command2 _ ->
            2

        CommandN commandN ->
            commandN.numberOfDefaultArguments
