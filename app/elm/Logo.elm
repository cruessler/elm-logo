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

import Compiler.Ast as Ast
import Compiler.Linker as Linker
import Compiler.Parser as Parser
import Dict
import Environment exposing (Environment)
import Environment.History exposing (History)
import Parser.Advanced as Parser exposing (DeadEnd)
import Vm.Vm as Vm exposing (State(..), Vm)


type Logo
    = Logo State


type Error context problem
    = ParseError (List (DeadEnd context problem))


empty : Logo
empty =
    Logo <| Done Vm.empty


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
            vm.compiledFunctions
                |> List.map (\function -> ( function.name, function ))
                |> Dict.fromList
                |> Parser.withExistingFunctions

        compiledProgram =
            program
                |> Parser.run parser
                |> Result.mapError ParseError
                |> Result.map Ast.compileProgram

        result =
            compiledProgram
                |> Result.map (Linker.linkProgram vm.compiledFunctions)
                |> Result.map Vm.initialize
                |> Result.map (Vm.withEnvironment vm.environment)
    in
    case result of
        Ok newVm ->
            Logo <| Paused { newVm | environment = Environment.input program newVm.environment }

        Err error ->
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
