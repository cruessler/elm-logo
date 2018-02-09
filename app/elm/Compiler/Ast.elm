module Compiler.Ast
    exposing
        ( Node(..)
        , Program
        , Function
        , compileProgram
        , compile
        )

{-| This module provides types and functions for working with Logo ASTs.
-}

import Dict exposing (Dict)
import Vm.Command as C
import Vm.Introspect as I
import Vm.Primitive as P
import Vm.Type as Type
import Vm.Vm exposing (Vm, Instruction(..))


type alias Program =
    { functions : List Function
    , body : List Node
    }


type alias Function =
    { name : String
    , requiredArguments : List String
    , optionalArguments : List ( String, Node )
    , body : List Node
    }


type alias CompiledProgram =
    { instructions : List Instruction
    , functionTable : Dict String Int
    , startAddress : Int
    }


type alias CompiledFunction =
    { name : String
    , body : List Instruction
    }


type Node
    = Repeat Node (List Node)
    | Foreach Node (List Node)
    | If Node (List Node)
    | Command1 C.Command1 Node
    | Primitive1 P.Primitive1 Node
    | Primitive2 P.Primitive2 Node Node
    | Introspect0 (I.Introspect0 Vm)
    | Introspect1 (I.Introspect1 Vm) Node
    | Call String (List Node)
    | Make String Node
    | Variable String
    | Value Type.Value


{-| Compile an AST node to a list of VM instructions.
-}
compile : Node -> List Instruction
compile node =
    case node of
        Repeat times children ->
            let
                compiledTimes =
                    compile times

                compiledChildren =
                    List.concatMap compile children
            in
                [ [ PushLoopScope ]
                , compiledTimes
                , [ Vm.Vm.Introspect0 { name = "repcount", f = I.repcount }
                  , Eval2 { name = "lessThan", f = P.lessThan }
                  , JumpIfFalse ((List.length compiledChildren) + 3)
                  , EnterLoopScope
                  ]
                , compiledChildren
                , [ Jump ((List.length compiledChildren) + 5 |> negate)
                  , PopLoopScope
                  ]
                ]
                    |> List.concat

        Foreach iterator children ->
            let
                compiledIterator =
                    compile iterator

                compiledChildren =
                    List.concatMap compile children
            in
                [ compiledIterator
                , [ PushTemplateScope
                  , PushValue (Type.Word "rest")
                  , Vm.Vm.Introspect1 { name = "?", f = I.templateVariable }
                  , Eval1 { name = "emptyp", f = P.emptyp }
                  , JumpIfTrue ((List.length compiledChildren) + 3)
                  , EnterTemplateScope
                  ]
                , compiledChildren
                , [ Jump ((List.length compiledChildren) + 5 |> negate)
                  , PopTemplateScope
                  ]
                ]
                    |> List.concat

        If condition children ->
            let
                compiledCondition =
                    compile condition

                compiledChildren =
                    List.concatMap compile children
            in
                [ compiledCondition
                , [ JumpIfFalse ((List.length compiledChildren) + 1) ]
                , compiledChildren
                ]
                    |> List.concat

        Command1 c node ->
            [ (compile node)
            , [ Vm.Vm.Command1 c ]
            ]
                |> List.concat

        Primitive1 p node ->
            [ (compile node)
            , [ Eval1 p ]
            ]
                |> List.concat

        Primitive2 p first second ->
            [ (compile second)
            , (compile first)
            , [ Eval2 p ]
            ]
                |> List.concat

        Introspect0 i ->
            [ Vm.Vm.Introspect0 i ]

        Introspect1 i node ->
            [ compile node
            , [ Vm.Vm.Introspect1 i ]
            ]
                |> List.concat

        Call name arguments ->
            let
                mangledName =
                    name ++ (toString <| List.length arguments)
            in
                [ List.reverse arguments |> List.concatMap compile
                , [ Vm.Vm.CallByName mangledName ]
                ]
                    |> List.concat

        Make name node ->
            [ compile node
            , [ StoreVariable name ]
            ]
                |> List.concat

        Variable name ->
            [ PushVariable name ]

        Value value ->
            [ PushValue value ]


compileProgram : Program -> CompiledProgram
compileProgram { functions, body } =
    let
        compiledFunctions =
            List.map compileFunction functions

        ( functionTable, startAddress ) =
            List.foldl
                (\f ( acc, address ) ->
                    ( Dict.insert f.name address acc
                    , address + List.length f.body
                    )
                )
                ( Dict.empty, 0 )
                compiledFunctions

        instructions =
            List.append
                (List.concatMap .body compiledFunctions)
                (List.concatMap compile body)
    in
        { instructions = instructions
        , functionTable = functionTable
        , startAddress = startAddress
        }


compileFunction : Function -> CompiledFunction
compileFunction { name, requiredArguments, body } =
    let
        instructions =
            [ [ PushLocalScope ]
            , List.concatMap (\arg -> [ LocalVariable arg, StoreVariable arg ]) requiredArguments
            , List.concatMap compile body
            , [ PopLocalScope
              , Return
              ]
            ]
                |> List.concat
    in
        { name = name ++ (toString <| List.length requiredArguments)
        , body = instructions
        }
