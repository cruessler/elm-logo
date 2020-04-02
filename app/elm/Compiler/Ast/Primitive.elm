module Compiler.Ast.Primitive exposing
    ( Primitive(..)
    , all
    , arguments
    , find
    , name
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
    , Primitive1 { name = "integerp", f = P.integerp }
    , Primitive2 { name = "equalp", f = P.equalp }
    , Primitive2 { name = "notequalp", f = P.notequalp }
    , Primitive1 { name = "minus", f = P.minus }
    , Primitive2 { name = "sum", f = P.sum }
    , Primitive2 { name = "difference", f = P.difference }
    , Primitive2 { name = "product", f = P.product }
    , Primitive2 { name = "quotient", f = P.quotient }
    , Primitive2 { name = "greaterp", f = P.greaterp }
    , Primitive2 { name = "lessp", f = P.lessp }
    , Primitive2 { name = "remainder", f = P.remainder }
    , Primitive2 { name = "sentence", f = P.sentence }
    ]


find : String -> Maybe Primitive
find name_ =
    List.filter (\x -> name x == name_) all
        |> List.head


name : Primitive -> String
name primitive =
    case primitive of
        Primitive1 primitive1 ->
            primitive1.name

        Primitive2 primitive2 ->
            primitive2.name


arguments : Primitive -> Int
arguments primitive =
    case primitive of
        Primitive1 _ ->
            1

        Primitive2 _ ->
            2
