module Compiler.Ast.Primitive exposing
    ( Primitive(..)
    , all
    , find
    , name
    , numberOfDefaultArguments
    )

import Vm.Primitive as P


type Primitive
    = Primitive1 P.Primitive1
    | Primitive2 P.Primitive2
    | Primitive3 P.Primitive3
    | PrimitiveN P.PrimitiveN


all : List Primitive
all =
    [ Primitive1 { name = "butfirst", f = P.butfirst }
    , Primitive1 { name = "bf", f = P.butfirst }
    , Primitive1 { name = "emptyp", f = P.emptyp }
    , Primitive1 { name = "empty?", f = P.emptyp }
    , Primitive1 { name = "first", f = P.first }
    , Primitive1 { name = "wordp", f = P.wordp }
    , Primitive1 { name = "listp", f = P.listp }
    , Primitive1 { name = "integerp", f = P.integerp }
    , Primitive1 { name = "floatp", f = P.floatp }
    , Primitive2 { name = "equalp", f = P.equalp }
    , Primitive2 { name = "equal?", f = P.equalp }
    , Primitive2 { name = "notequalp", f = P.notequalp }
    , Primitive2 { name = "notequal?", f = P.notequalp }
    , Primitive1 { name = "minus", f = P.minus }
    , PrimitiveN { name = "sum", f = P.sum, numberOfDefaultArguments = 2 }
    , Primitive2 { name = "difference", f = P.difference }
    , Primitive2 { name = "product", f = P.product }
    , Primitive2 { name = "quotient", f = P.quotient }
    , Primitive1 { name = "round", f = P.round_ }
    , Primitive2 { name = "greaterp", f = P.greaterp }
    , Primitive2 { name = "greater?", f = P.greaterp }
    , Primitive2 { name = "lessp", f = P.lessp }
    , Primitive2 { name = "less?", f = P.lessp }
    , Primitive2 { name = "remainder", f = P.remainder }
    , Primitive2 { name = "fput", f = P.fput }
    , Primitive2 { name = "lput", f = P.lput }
    , PrimitiveN { name = "array", f = P.array, numberOfDefaultArguments = 1 }
    , PrimitiveN { name = "sentence", f = P.sentence, numberOfDefaultArguments = 2 }
    , PrimitiveN { name = "se", f = P.sentence, numberOfDefaultArguments = 2 }
    , Primitive2 { name = "word", f = P.word }
    , Primitive1 { name = "count", f = P.count }
    , Primitive1 { name = "char", f = P.char }
    , Primitive3 { name = "form", f = P.form }
    , PrimitiveN { name = "bitand", f = P.bitand, numberOfDefaultArguments = 2 }
    , Primitive1 { name = "bitnot", f = P.bitnot }
    , Primitive2 { name = "ashift", f = P.ashift }
    , Primitive2 { name = "lshift", f = P.lshift }
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

        Primitive3 primitive3 ->
            primitive3.name

        PrimitiveN primitiveN ->
            primitiveN.name


numberOfDefaultArguments : Primitive -> Int
numberOfDefaultArguments primitive =
    case primitive of
        Primitive1 _ ->
            1

        Primitive2 _ ->
            2

        Primitive3 _ ->
            3

        PrimitiveN primitiveN ->
            primitiveN.numberOfDefaultArguments
