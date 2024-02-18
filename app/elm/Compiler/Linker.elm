module Compiler.Linker exposing (LinkedProgram, linkProgram)

import Compiler.Ast exposing (CompiledFunction, CompiledMacro, CompiledProgram)
import Dict exposing (Dict)
import Vm.Instruction exposing (Instruction)


{-| Represent a linked program.
-}
type alias LinkedProgram =
    { instructions : List Instruction
    , functionTable : Dict String Int
    , compiledFunctions : List CompiledFunction
    , compiledMacros : List CompiledMacro
    , startAddress : Int
    }


linkProgram : List CompiledFunction -> List CompiledMacro -> CompiledProgram -> LinkedProgram
linkProgram existingCompiledFunctions existingCompiledMacros program =
    let
        compiledFunctions =
            program.compiledFunctions
                |> List.append existingCompiledFunctions

        compiledFunctionInstances =
            List.concatMap .instances compiledFunctions

        ( functionTable, startAddressAfterFunctions ) =
            List.foldl
                (\f ( acc, address ) ->
                    ( Dict.insert f.mangledName address acc
                    , address + List.length f.body
                    )
                )
                ( Dict.empty, 0 )
                compiledFunctionInstances

        compiledMacros =
            program.compiledMacros
                |> List.append existingCompiledMacros

        ( macroAndFunctionTable, startAddressAfterMacros ) =
            List.foldl
                (\m ( acc, address ) ->
                    ( Dict.insert m.name address acc
                    , address + List.length m.body
                    )
                )
                ( functionTable, startAddressAfterFunctions )
                compiledMacros

        instructionsForFunctions =
            List.concatMap .body compiledFunctionInstances

        instructionsForMacros =
            List.concatMap .body compiledMacros

        instructions =
            [ instructionsForFunctions
            , instructionsForMacros
            , program.instructions
            ]
                |> List.concat
    in
    { instructions = instructions
    , functionTable = macroAndFunctionTable
    , compiledFunctions = compiledFunctions
    , compiledMacros = compiledMacros
    , startAddress = startAddressAfterMacros
    }
