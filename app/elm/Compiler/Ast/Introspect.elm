module Compiler.Ast.Introspect exposing
    ( Introspect(..)
    , all
    , arguments
    , find
    , name
    , templateVariable
    )

import Vm.Introspect as I


type Introspect
    = Introspect0 I.Introspect0
    | Introspect1 I.Introspect1


all : List Introspect
all =
    [ Introspect0 { name = "repcount", f = I.repcount }
    ]


find : String -> Maybe Introspect
find name_ =
    List.filter (\x -> name x == name_) all
        |> List.head


name : Introspect -> String
name introspect =
    case introspect of
        Introspect0 introspect1 ->
            introspect1.name

        Introspect1 introspect2 ->
            introspect2.name


arguments : Introspect -> Int
arguments introspect =
    case introspect of
        Introspect0 _ ->
            0

        Introspect1 _ ->
            1


templateVariable : I.Introspect1
templateVariable =
    { name = "?", f = I.templateVariable }
