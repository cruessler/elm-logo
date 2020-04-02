module Logo exposing
    ( Logo
    , continue
    , done
    , empty
    , getEnvironment
    , getHistory
    , getVm
    , run
    , step
    )

import Array exposing (Array)
import Compiler.Ast as Ast
import Compiler.Parser as Parser
import Environment exposing (Environment)
import Environment.History exposing (Entry)
import Parser.Advanced as Parser exposing (DeadEnd)
import Vm.Error as Error
import Vm.Vm as Vm exposing (Instruction, State(..), Vm)


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
        Paused vm ->
            False

        Done _ ->
            True


compile : String -> Logo -> Logo
compile program logo =
    let
        vm =
            getVm logo

        loadProgram : Ast.CompiledProgram -> Vm
        loadProgram { instructions, functionTable, startAddress } =
            let
                env =
                    Environment.input program vm.environment
            in
            Vm.initialize instructions functionTable startAddress
                |> Vm.setEnvironment env

        result =
            program
                |> Parser.run Parser.root
                |> Result.mapError ParseError
                |> Result.map Ast.compileProgram
                |> Result.map loadProgram
    in
    case result of
        Ok newVm ->
            Logo <| Paused newVm

        Err error ->
            Logo <| Done { vm | environment = Environment.error (Debug.toString error) vm.environment }


step : Logo -> Logo
step (Logo state) =
    case state of
        Paused vm ->
            case Vm.step vm of
                Ok newVm ->
                    Logo <| Paused newVm

                Err error ->
                    Logo <|
                        Done
                            { vm
                                | environment = Environment.error (Error.toString error) vm.environment
                            }

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


getHistory : Logo -> List Entry
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
