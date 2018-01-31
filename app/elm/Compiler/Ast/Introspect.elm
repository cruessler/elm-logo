module Compiler.Ast.Introspect
    exposing
        ( Introspect(..)
        , all
        , find
        , arguments
        , templateVariable
        )

import Vm.Introspect as I
import Vm.Vm exposing (Vm)


type Introspect
    = Introspect0 (I.Introspect0 Vm)
    | Introspect1 (I.Introspect1 Vm)


all : List Introspect
all =
    [ Introspect0 { name = "repcount", f = I.repcount }
    ]


find : String -> Maybe Introspect
find name_ =
    List.filter (\x -> name x == name_) all
        |> List.head


name : Introspect -> String
name command =
    case command of
        Introspect0 { name } ->
            name

        Introspect1 { name } ->
            name


arguments : Introspect -> Int
arguments command =
    case command of
        Introspect0 _ ->
            0

        Introspect1 _ ->
            1


templateVariable : I.Introspect1 Vm
templateVariable =
    { name = "?", f = I.templateVariable }
