module Compiler.Ast.Primitive
    exposing
        ( Primitive(..)
        , all
        , find
        , arguments
        )

import Vm.Primitive as P
import Vm.Vm exposing (Vm)


type Primitive
    = Primitive1 P.Primitive1
    | Primitive2 P.Primitive2


all : List Primitive
all =
    [ Primitive1 { name = "butfirst", f = P.butfirst }
    , Primitive1 { name = "emptyp", f = P.emptyp }
    , Primitive1 { name = "first", f = P.first }
    , Primitive2 { name = "sentence", f = P.sentence }
    ]


find : String -> Maybe Primitive
find name_ =
    List.filter (\x -> name x == name_) all
        |> List.head


name : Primitive -> String
name command =
    case command of
        Primitive1 { name } ->
            name

        Primitive2 { name } ->
            name


arguments : Primitive -> Int
arguments command =
    case command of
        Primitive1 _ ->
            1

        Primitive2 _ ->
            2
