module Test.Parser exposing (..)

import Compiler.Ast as Ast
import Compiler.Parser as Parser
import Expect exposing (Expectation)
import Parser
import Test exposing (..)
import Vm.Type as Type


value : Test
value =
    let
        integer =
            Parser.run Parser.value "1234"

        word =
            Parser.run Parser.value "\"word"

        list =
            Parser.run Parser.value "[ 1234 \"word ]"

        nestedList =
            Parser.run Parser.value "[ 1234 [ 1234 ] ]"

        fail =
            Parser.run Parser.value " [ 1 2"
    in
        describe "parse simple values" <|
            [ test "parse integer" <|
                \_ -> Expect.equal integer (Ok <| Ast.Value <| Type.Int 1234)
            , test "parse word" <|
                \_ -> Expect.equal word (Ok <| Ast.Value <| Type.Word "word")
            , test "parse list" <|
                \_ ->
                    Expect.equal list
                        (Ok <| Ast.Value <| Type.List [ Type.Int 1234, Type.Word "word" ])
            , test "parse nested list" <|
                \_ ->
                    Expect.equal nestedList
                        (Ok <|
                            Ast.Value <|
                                Type.List [ Type.Int 1234, Type.List [ Type.Int 1234 ] ]
                        )
            , test "fail when given string cannot be parsed" <|
                \_ -> Expect.err fail
            ]
