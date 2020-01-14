module Test.Parser exposing (..)

import Compiler.Ast as Ast
import Compiler.Parser as Parser
import Compiler.Parser.Value as Value
import Dict
import Expect exposing (Expectation)
import Parser
import Test exposing (..)
import Vm.Command as C
import Vm.Type as Type


value : Test
value =
    let
        integer =
            Parser.run Value.value "1234"

        word =
            Parser.run Value.value "\"word"

        list =
            Parser.run Value.value "[ 1234 word ]"

        nestedList =
            Parser.run Value.value "[ 1234 [ 1234 ] ]"

        emptyList =
            Parser.run Value.value "[ ]"

        fail =
            Parser.run Value.value " [ 1 2"
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
            , test "parse empty list" <|
                \_ ->
                    Expect.equal emptyList
                        (Ok <| Ast.Value <| Type.List [])
            , test "fail when given string cannot be parsed" <|
                \_ -> Expect.err fail
            ]


parsesFunction : String -> Ast.Function -> Expectation
parsesFunction source function =
    let
        state =
            { inFunction = False, userDefinedFunctions = Dict.empty }

        result =
            Parser.run (Parser.functionDefinition state) source
    in
        Expect.equal result (Ok function)


functionDefinition : Test
functionDefinition =
    describe "define a function" <|
        [ test "with mandatory arguments and no body" <|
            \_ ->
                parsesFunction "to foo :bar :baz\nend\n"
                    { name = "foo"
                    , requiredArguments = [ "bar", "baz" ]
                    , optionalArguments = []
                    , body = []
                    }
        , test "with mandatory argument and body" <|
            \_ ->
                parsesFunction "to foo :bar\nprint :bar\nprint :baz\nend\n"
                    { name = "foo"
                    , requiredArguments = [ "bar" ]
                    , optionalArguments = []
                    , body =
                        [ Ast.Command1
                            { name = "print", f = C.print }
                            (Ast.Variable "bar")
                        , Ast.Command1
                            { name = "print", f = C.print }
                            (Ast.Variable "baz")
                        ]
                    }
        , test "with optional arguments" <|
            \_ ->
                parsesFunction "to foo :bar [:baz \"baz]\nprint :bar\nend\n"
                    { name = "foo"
                    , requiredArguments = [ "bar" ]
                    , optionalArguments = [ ( "baz", Ast.Value <| Type.Word <| "baz" ) ]
                    , body =
                        [ Ast.Command1
                            { name = "print", f = C.print }
                            (Ast.Variable "bar")
                        ]
                    }
        , test "without arguments" <|
            \_ ->
                parsesFunction "to foo\nend\n"
                    { name = "foo"
                    , requiredArguments = []
                    , optionalArguments = []
                    , body = []
                    }
        ]
