module Test.Error exposing (..)

import Compiler.Ast as Ast
import Compiler.Parser as Parser
import Expect exposing (Expectation)
import Parser
import Test exposing (Test, describe, test)
import Vm.Error as Error
import Vm.Vm as Vm


match : Result String Vm.Vm -> String -> Expectation
match result expected =
    case result of
        Ok { environment } ->
            Expect.fail <|
                "Expected to fail with error `"
                    ++ expected
                    ++ "`, but program ran successfully"

        Err actual ->
            Expect.equal actual expected


failsWithMessage : String -> String -> Test
failsWithMessage program actual =
    let
        result =
            program
                |> Parser.run Parser.root
                |> Result.mapError toString
                |> Result.map Ast.compileProgram
                |> Result.map
                    (\{ instructions, functionTable, startAddress } ->
                        Vm.initialize instructions functionTable startAddress
                    )
                |> Result.andThen (Vm.run >> Result.mapError Error.toString)
    in
        test program <|
            \_ -> match result actual


repeatWithInvalidArguments : Test
repeatWithInvalidArguments =
    describe "repeat fails given non-integer argument" <|
        [ failsWithMessage "repeat \"foo []" "repeat doesn’t like foo as input"
        ]


functionsWithInvalidArguments : Test
functionsWithInvalidArguments =
    describe "functions fail given argument of wrong type" <|
        [ failsWithMessage "print butfirst []" "butfirst doesn’t like  as input"
        , failsWithMessage "print first []" "first doesn’t like  as input"
        , failsWithMessage "print first butfirst \"a" "first doesn’t like  as input"
        , failsWithMessage "print lessp \"word \"word" "lessp doesn’t like word as input"
        , failsWithMessage "if \"word [ print \"word ]" "if doesn’t like word as input"
        ]


noOutput : Test
noOutput =
    describe "prints error if no value is returned" <|
        [ failsWithMessage """to foo :bar
print "bar
end
print foo "baz""" "foo did not output to print"
        , failsWithMessage """to foo :bar
repeat 4 [ print 4 ]
end
print foo "baz""" "foo did not output to print"
        , failsWithMessage "print print 3" "print did not output to print"
        , failsWithMessage "print print print 3" "print did not output to print"
        , failsWithMessage "if print 3 [ print 3 ]" "print did not output to if"
        , failsWithMessage "print if \"true []" "if did not output to print"
        , failsWithMessage "print repeat 1 [ print 1 ]" "repeat did not output to print"
        , failsWithMessage "print foreach 1 [ print 1 ]" "foreach did not output to print"
        , failsWithMessage """print make "foo "bar""" "make did not output to print"
        ]


outputOutsideProcedure : Test
outputOutsideProcedure =
    describe "prints error if output is used outside procedure" <|
        [ failsWithMessage """to foo :bar [:baz output 5]
print :bar
end
foo "bar""" "Can only use output inside a procedure"
        , failsWithMessage "output 5" "Can only use output inside a procedure"
        ]


noUseOfValue : Test
noUseOfValue =
    describe "prints error if value is not passed to function" <|
        [ failsWithMessage "3" "You don’t say what to do with 3"
        , failsWithMessage "repcount" "You don’t say what to do with -1"
        , failsWithMessage "foreach 1 [ 5 ]" "You don’t say what to do with 5"
        , failsWithMessage "repeat 1 [ 5 ]" "You don’t say what to do with 5"
        , failsWithMessage "if \"true [ 5 ]" "You don’t say what to do with 5"
        ]
