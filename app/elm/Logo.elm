module Logo exposing
    ( Logo
    , compile
    , continue
    , done
    , empty
    , getEnvironment
    , getHistory
    , getVm
    , run
    , step
    )

import Compiler.Ast as Ast exposing (Context(..))
import Compiler.Linker as Linker
import Compiler.Parser as Parser
import Environment exposing (Environment)
import Environment.History exposing (History)
import Parser.Advanced as Parser
import StandardLibrary
import Vm.Vm as Vm exposing (State(..), Vm)


type Logo
    = Logo State


type Error
    = ParseError Parser.Error


empty : Logo
empty =
    let
        vm =
            Vm.empty
                |> Vm.withCompiledFunctions StandardLibrary.compiledFunctions
    in
    Logo <| Done vm


continue : Logo -> Logo
continue (Logo state) =
    case state of
        Paused vm ->
            Logo <| Vm.run vm

        Done _ ->
            Logo state


done : Logo -> Bool
done (Logo state) =
    case state of
        Paused _ ->
            False

        Done _ ->
            True


compile : String -> Logo -> Logo
compile program logo =
    let
        vm =
            getVm logo

        parser =
            Vm.getParser vm

        compiledProgram =
            program
                |> Parser.run parser
                |> Result.mapError ParseError
                |> Result.map (Ast.compileProgram Statement)

        linkProgram =
            Linker.linkProgram vm.compiledFunctions vm.compiledMacros

        result =
            compiledProgram
                |> Result.map linkProgram
                |> Result.map Vm.initialize
                |> Result.map (Vm.withEnvironment vm.environment)
    in
    case result of
        Ok newVm ->
            Logo <| Paused { newVm | environment = Environment.input program newVm.environment }

        Err _ ->
            Logo <| Done { vm | environment = Environment.error "parse error" vm.environment }


step : Logo -> Logo
step (Logo state) =
    case state of
        Paused vm ->
            Logo <| Vm.step vm

        Done _ ->
            Logo state


run : String -> Logo -> Logo
run program logo =
    compile program logo |> continue


getEnvironment : Logo -> Environment
getEnvironment (Logo state) =
    case state of
        Paused { environment } ->
            environment

        Done { environment } ->
            environment


getHistory : Logo -> History
getHistory (Logo state) =
    case state of
        Paused { environment } ->
            environment.history

        Done { environment } ->
            environment.history


getVm : Logo -> Vm
getVm (Logo state) =
    case state of
        Paused vm ->
            vm

        Done vm ->
            vm
