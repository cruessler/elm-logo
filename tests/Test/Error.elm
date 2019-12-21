module Test.Error exposing (..)

import Compiler.Ast as Ast
import Compiler.Parser as Parser
import Expect exposing (Expectation)
import Parser
import Test exposing (Test, describe, test)
import Vm.Error
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
        , failsWithMessage "print butfirst []" <| Runtime <| Vm.Error.WrongInput "butfirst" ""
        , failsWithMessage "print first []" <| Runtime <| Vm.Error.WrongInput "first" ""
        , failsWithMessage "print first butfirst \"a" <| Runtime <| Vm.Error.WrongInput "first" ""
        , failsWithMessage "print lessp \"word \"word" <| Runtime <| Vm.Error.WrongInput "lessp" "word"
        ]
