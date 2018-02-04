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


functionDefinitions : Test
functionDefinitions =
    describe "define unused function and print words" <|
        [ printsLines
            """to foo :bar
print :bar
print :bar
end
print "baz print "baz"""
            [ "baz", "baz" ]
        ]