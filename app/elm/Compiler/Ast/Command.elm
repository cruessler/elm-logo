module Compiler.Ast.Command
    exposing
        ( Command(..)
        , all
        , find
        , arguments
        )

import Vm.Command as C


type Command
    = Command1 C.Command1
    | Command2 C.Command2


all : List Command
all =
    [ Command1 { name = "print", f = C.print }
    ]


find : String -> Maybe Command
find name_ =
    List.filter (\x -> name x == name_) all
        |> List.head


name : Command -> String
name command =
    case command of
        Command1 { name } ->
            name

        Command2 { name } ->
            name


arguments : Command -> Int
arguments command =
    case command of
        Command1 _ ->
            1

        Command2 _ ->
            2
