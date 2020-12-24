module Test.Parser exposing (..)

import Compiler.Ast as Ast
import Compiler.Parser as Parser
import Compiler.Parser.Value as Value
import Dict
import Expect exposing (Expectation)
import Parser.Advanced as Parser exposing (DeadEnd)
import Test exposing (..)
import Vm.Command as C
import Vm.Type as Type


defaultState : Parser.State
defaultState =
    { userDefinedFunctions = Dict.empty
    , parsedBody = []
    , inFunction = False
    }


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
        result =
            Parser.run (Parser.functionDefinition defaultState) source
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


parsesArithmeticExpression : String -> Test
parsesArithmeticExpression expression =
    let
        match : Result (List (DeadEnd context problem)) Ast.Node -> Expectation
        match result =
            case result of
                Ok _ ->
                    Expect.pass

                Err _ ->
                    Expect.fail <| "could not parse expression \"" ++ expression ++ "\""
    in
    test expression <|
        \_ ->
            match
                (Parser.run
                    (Parser.arithmeticExpression defaultState)
                    expression
                )


arithmetic : Test
arithmetic =
    describe "arithmetic" <|
        [ parsesArithmeticExpression "5 * 5"
        , parsesArithmeticExpression "5*5"
        , parsesArithmeticExpression "(5 * 5)"
        , parsesArithmeticExpression "(5*5)"
        , parsesArithmeticExpression "((5 * 5))"
        , parsesArithmeticExpression "5"
        , parsesArithmeticExpression "4 - 4"
        , parsesArithmeticExpression "4 - 4 + 4 - 4"
        , parsesArithmeticExpression "5 / 10"
        , parsesArithmeticExpression "5 / 10 + 10"
        , parsesArithmeticExpression "10 + 10 * 10"
        , parsesArithmeticExpression "10 + 10 + 10"
        , parsesArithmeticExpression "((10 + 10) + 10)"
        , parsesArithmeticExpression "(10 + 10 + 10)"
        , parsesArithmeticExpression "10 * 10 * 10"
        , parsesArithmeticExpression "(10 * 10 * 10)"
        , parsesArithmeticExpression "((10 * 10) * 10)"
        , parsesArithmeticExpression ":depth - 1"
        , parsesArithmeticExpression ":depth / 1"
        , parsesArithmeticExpression "(:depth - 1)"
        , parsesArithmeticExpression "(sum 1 1)"
        , parsesArithmeticExpression "(? 1)"
        , parsesArithmeticExpression "(minus :size/2) + 5"
        , parsesArithmeticExpression "(minus :size+2) + 5"
        , parsesArithmeticExpression "(:zr + :az*:az)"
        , parsesArithmeticExpression "(:az*:az - :bz*:bz)"
        , parsesArithmeticExpression "(:zr + :az*:az - :bz*:bz)"
        , parsesArithmeticExpression "(sum :zr :zi)"
        , parsesArithmeticExpression "(sum :zr (1))"
        , parsesArithmeticExpression "(sum :zr (sum 1 1))"
        , parsesArithmeticExpression "(sum :zr (1 + 1))"
        , parsesArithmeticExpression "(sum :zr (:count + 1))"
        , parsesArithmeticExpression "(sum (:count + 1) (:zi + 2*:bar))"
        ]


parsesBooleanExpression : String -> Test
parsesBooleanExpression expression =
    let
        match : Result (List (DeadEnd context problem)) Ast.Node -> Expectation
        match result =
            case result of
                Ok _ ->
                    Expect.pass

                Err _ ->
                    Expect.fail <| "could not parse expression \"" ++ expression ++ "\""
    in
    test expression <|
        \_ ->
            match
                (Parser.run
                    (Parser.booleanExpression defaultState)
                    expression
                )


booleanAlgebra : Test
booleanAlgebra =
    describe "boolean algebra" <|
        [ parsesBooleanExpression "1 + 1 = 1"
        , parsesBooleanExpression "1 <> 1"
        , parsesBooleanExpression "2 * (3 + 1) = (4 + 4) * 7"
        ]
