module Test.Error exposing (..)

import Compiler.Ast as Ast
import Compiler.Parser as Parser
import Expect exposing (Expectation)
import Parser
import Test exposing (Test, describe, test)
import Vm.Error
import Vm.Exception as Exception
import Vm.Vm as Vm


type Error
    = Parse Parser.Error
    | Runtime Vm.Error.Error


match : Result Error Vm.Vm -> Error -> Expectation
match result expected =
    case result of
        Ok { environment } ->
            Expect.fail <|
                "Expected to fail with error `"
                    ++ (toString expected)
                    ++ "`, but program ran successfully"

        Err actual ->
            Expect.equal actual expected


failsWithMessage : String -> Error -> Test
failsWithMessage program actual =
    let
        result =
            program
                |> Parser.run Parser.root
                |> Result.mapError Parse
                |> Result.map Ast.compileProgram
                |> Result.map
                    (\{ instructions, functionTable, startAddress } ->
                        Vm.initialize instructions functionTable startAddress
                    )
                |> Result.andThen (Vm.run >> Result.mapError Runtime)
    in
        test program <|
            \_ -> match result actual


repeatWithInvalidArguments : Test
repeatWithInvalidArguments =
    describe "repeat fails given non-integer argument" <|
        [ failsWithMessage "repeat \"foo []" <| Runtime <| Vm.Error.WrongInput "repeat" "foo"
        ]


functionsWithInvalidArguments : Test
functionsWithInvalidArguments =
    describe "functions fail given argument of wrong type" <|
        [ failsWithMessage "print butfirst []" <| Runtime <| Vm.Error.WrongInput "butfirst" ""
        , failsWithMessage "print first []" <| Runtime <| Vm.Error.WrongInput "first" ""
        , failsWithMessage "print first butfirst \"a" <| Runtime <| Vm.Error.WrongInput "first" ""
        , failsWithMessage "print lessp \"word \"word" <| Runtime <| Vm.Error.WrongInput "lessp" "word"
        ]


noOutput : Test
noOutput =
    describe "prints error if no value is returned" <|
        [ failsWithMessage """to foo :bar
print "bar
end
print foo "baz""" <| Runtime <| Vm.Error.Exception <| Exception.NoOutput "print" "foo"
        , failsWithMessage """to foo :bar
repeat 4 [ print 4 ]
end
print foo "baz""" <| Runtime <| Vm.Error.Exception <| Exception.NoOutput "print" "foo"
        , failsWithMessage "print print 3" <| Runtime <| Vm.Error.Exception <| Exception.NoOutput "print" "print"
        , failsWithMessage "print print print 3" <| Runtime <| Vm.Error.Exception <| Exception.NoOutput "print" "print"
        , failsWithMessage "if print 3 [ print 3 ]" <| Runtime <| Vm.Error.Exception <| Exception.NoOutput "if" "print"
        , failsWithMessage "print if \"true []" <| Runtime <| Vm.Error.Exception <| Exception.NoOutput "print" "if"
        , failsWithMessage "print repeat 1 [ print 1 ]" <| Runtime <| Vm.Error.Exception <| Exception.NoOutput "print" "repeat"
        , failsWithMessage "print foreach 1 [ print 1 ]" <| Runtime <| Vm.Error.Exception <| Exception.NoOutput "print" "foreach"
        , failsWithMessage """print make "foo "bar""" <| Runtime <| Vm.Error.Exception <| Exception.NoOutput "print" "make"
        ]


outputOutsideProcedure : Test
outputOutsideProcedure =
    describe "prints error if output is used outside procedure" <|
        [ failsWithMessage """to foo :bar [:baz output 5]
print :bar
end
foo "bar""" <| Runtime <| Vm.Error.Exception <| Exception.OutputOutsideFunction
        , failsWithMessage "output 5" <| Runtime <| Vm.Error.Exception <| Exception.OutputOutsideFunction
        ]


noUseOfValue : Test
noUseOfValue =
    describe "prints error if value is not passed to function" <|
        [ failsWithMessage "3" <| Runtime <| Vm.Error.NoUseOfValue "3"
        , failsWithMessage "repcount" <| Runtime <| Vm.Error.NoUseOfValue "-1"
        , failsWithMessage "foreach 1 [ 5 ]" <| Runtime <| Vm.Error.NoUseOfValue "5"
        , failsWithMessage "repeat 1 [ 5 ]" <| Runtime <| Vm.Error.NoUseOfValue "5"
        , failsWithMessage "if \"true [ 5 ]" <| Runtime <| Vm.Error.NoUseOfValue "5"
        ]
