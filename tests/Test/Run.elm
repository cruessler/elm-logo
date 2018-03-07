module Test.Run exposing (..)

import Array
import Compiler.Ast as Ast
import Compiler.Parser as Parser
import Expect exposing (Expectation)
import Parser
import Test exposing (Test, describe, test)
import Vm.Type as Type
import Vm.Vm


printsLines : String -> List String -> Test
printsLines program lines =
    let
        result =
            program
                |> Parser.run Parser.root
                |> Result.mapError
                    (\error ->
                        "Error parsing `"
                            ++ program
                            ++ "`. The error was: "
                            ++ (toString error)
                    )
                |> Result.map Ast.compileProgram
                |> Result.map
                    (\{ instructions, functionTable, startAddress } ->
                        Vm.Vm.initialize instructions functionTable startAddress
                    )
                |> Result.andThen Vm.Vm.run

        match : Result String Vm.Vm.Vm -> Expectation
        match result =
            case result of
                Ok { environment } ->
                    Expect.equal
                        (lines |> Array.fromList)
                        environment.lines

                Err message ->
                    Expect.fail message
    in
        test program <|
            \_ -> match result


failsParsing : String -> String -> Test
failsParsing program message =
    let
        result =
            Parser.run Parser.root program

        match result =
            case result of
                Err error ->
                    Expect.equal error.problem (Parser.Fail message)

                _ ->
                    Expect.fail <|
                        "Expected to fail with error message `"
                            ++ message
                            ++ "`, but parsed program successfully"
    in
        test program <|
            \_ -> match result


statements : Test
statements =
    describe "call print several times" <|
        [ printsLines "print 1234 print \"word" [ "1234", "word" ]
        ]


statement : Test
statement =
    describe "call Logo command" <|
        [ printsLines "print \"word" [ "word" ]
        , printsLines "print 1234" [ "1234" ]
        ]


if_ : Test
if_ =
    describe "if" <|
        [ printsLines "if \"true [ print 1234 ]" [ "1234" ]
        , printsLines "if \"false [ print 1234 ]" []
        ]


repeat : Test
repeat =
    describe "repeat" <|
        [ printsLines "repeat 2 [ print 1234 ]" [ "1234", "1234" ]
        , printsLines "repeat 2 [ repeat 2 [ print \"word ] ]" (List.repeat 4 "word")
        ]


repeatWithRepcount : Test
repeatWithRepcount =
    describe "repeat with repcount " <|
        [ printsLines "repeat 4 [ print repcount ]" [ "1", "2", "3", "4" ]
        , printsLines "repeat 2 [ repeat 2 [ print repcount ] ]" [ "1", "2", "1", "2" ]
        ]


foreach : Test
foreach =
    describe "foreach" <|
        [ printsLines "foreach [ 1 2 3 4 ] [ print \"word ]" (List.repeat 4 "word")
        ]


foreachWithTemplateVariable : Test
foreachWithTemplateVariable =
    describe "foreach with template variable" <|
        [ printsLines "foreach [ 1 2 ] [ print ?rest ]" [ "2", "" ]
        , printsLines "foreach \"word [ print ?rest ]" [ "ord", "rd", "d", "" ]
        , printsLines "foreach \"word [ print ?1 ]" [ "w", "o", "r", "d" ]
        ]


repeatWithVariable : Test
repeatWithVariable =
    describe "repeat with variable" <|
        [ printsLines "make \"foo \"bar repeat 3 [ print :foo ]" (List.repeat 3 "bar")
        ]


printSeveralVariables : Test
printSeveralVariables =
    describe "print several variables" <|
        [ printsLines
            """make "foo "bar make "bar "bar make "baz "bar print :foo print :bar print :baz"""
            (List.repeat 3 "bar")
        ]


printContatenatedWords : Test
printContatenatedWords =
    describe "print words concatenated by `sentence`" <|
        [ printsLines
            """make "foo "bar make "bar "baz print sentence :foo :bar"""
            [ "bar baz" ]
        ]


{-| The programs in these tests are deliberately put at the beginning of a
line because the parser expects "end" to be the only thing in a line that
closes a function definition.
-}
functionDefinitions : Test
functionDefinitions =
    describe "define function and print words" <|
        [ printsLines
            """to foo :bar
print :bar
print :bar
end
print "baz print "baz"""
            [ "baz", "baz" ]
        , printsLines
            """to foo :bar
print :bar
print :bar
end
foo "baz"""
            [ "baz", "baz" ]
        , printsLines
            """to foo :bar
if emptyp :bar [ print "baz foo [ "baz ] ]
print "bar
end
foo []"""
            [ "baz", "bar", "bar" ]
        ]


variableFunctionCalls : Test
variableFunctionCalls =
    describe "define function and call it inside parentheses" <|
        [ printsLines
            """to foo :bar
print :bar
end
(foo "baz) (foo "baz)"""
            [ "baz", "baz" ]
        , failsParsing
            """to foo :bar
print :bar
end
(foo "bar "baz)"""
            "too many inputs to foo"
        , failsParsing
            """to foo :bar :baz
print :bar
end
(foo "bar)"""
            "not enough inputs to foo"
        ]


optionalArguments : Test
optionalArguments =
    describe "define function with default value for optional parameter" <|
        [ printsLines
            """to foo :bar [:baz "qux]
print :bar
print :baz
end
(foo "bar) (foo "bar "quux)"""
            [ "bar", "qux", "bar", "quux" ]
        , printsLines
            """to choices :menu [:sofar []]
if emptyp :menu [print :sofar stop]
foreach first :menu [(choices butfirst :menu sentence :sofar ?)]
end
choices [[small medium large] [vanilla [ultra chocolate] lychee [rum raisin] ginger] [cone cup]]
"""
            [ "small vanilla cone"
            , "small vanilla cup"
            , "small ultra chocolate cone"
            , "small ultra chocolate cup"
            , "small lychee cone"
            , "small lychee cup"
            , "small rum raisin cone"
            , "small rum raisin cup"
            , "small ginger cone"
            , "small ginger cup"
            , "medium vanilla cone"
            , "medium vanilla cup"
            , "medium ultra chocolate cone"
            , "medium ultra chocolate cup"
            , "medium lychee cone"
            , "medium lychee cup"
            , "medium rum raisin cone"
            , "medium rum raisin cup"
            , "medium ginger cone"
            , "medium ginger cup"
            , "large vanilla cone"
            , "large vanilla cup"
            , "large ultra chocolate cone"
            , "large ultra chocolate cup"
            , "large lychee cone"
            , "large lychee cup"
            , "large rum raisin cone"
            , "large rum raisin cup"
            , "large ginger cone"
            , "large ginger cup"
            ]
        ]
