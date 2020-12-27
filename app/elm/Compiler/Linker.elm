module Compiler.Linker exposing (LinkedProgram, linkProgram)

import Compiler.Ast exposing (CompiledFunction, CompiledProgram)
import Dict exposing (Dict)
import Vm.Instruction exposing (Instruction)


{-| Represent a linked program.
-}
type alias LinkedProgram =
    { instructions : List Instruction
    , functionTable : Dict String Int
    , compiledFunctions : List CompiledFunction
    , startAddress : Int
    }


linkProgram : List CompiledFunction -> CompiledProgram -> LinkedProgram
linkProgram existingCompiledFunctions program =
    let
        compiledFunctions =
            program.compiledFunctions
                |> List.append existingCompiledFunctions

        compiledFunctionInstances =
            List.concatMap .instances compiledFunctions

        ( functionTable, startAddress ) =
            List.foldl
                (\f ( acc, address ) ->
                    ( Dict.insert f.mangledName address acc
                    , address + List.length f.body
                    )
                )
                ( Dict.empty, 0 )
                compiledFunctionInstances

        instructions =
            List.append
                (List.concatMap .body compiledFunctionInstances)
                program.instructions
    in
    { instructions = instructions
    , functionTable = functionTable
    , compiledFunctions = compiledFunctions
    , startAddress = startAddress
    }
