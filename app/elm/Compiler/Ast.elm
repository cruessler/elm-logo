module Compiler.Ast
    exposing
        ( Node(..)
        , Program
        , Function
        , compileProgram
        , compileFunction
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


{-| Mangle the name of a function based on its number of arguments. This is
used to disambiguate variants of a function that take different numbers of
arguments.

The declaration

    to foo :bar [:baz "qux]
    end

would yield two functions, "foo1" and "foo2", which take 1 and 2 arguments,
respectively.

-}
mangleName : String -> Int -> String
mangleName name arguments =
    name ++ (toString arguments)


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
            List.concatMap compileFunction functions

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


compileRequiredArgument : String -> List Instruction
compileRequiredArgument arg =
    [ LocalVariable arg, StoreVariable arg ]


compileOptionalArgument : ( String, Node ) -> List Instruction
compileOptionalArgument ( arg, node ) =
    [ [ LocalVariable arg ]
    , compile node
    , [ StoreVariable arg ]
    ]
        |> List.concat


{-| Compile a function, taking optional arguments into account by returning
multiple versions, each taking a different number of arguments.
-}
compileFunction : Function -> List CompiledFunction
compileFunction { name, requiredArguments, optionalArguments, body } =
    let
        {- Instructions for required arguments. -}
        instructionsForRequiredArguments =
            [ [ PushLocalScope ]
            , List.concatMap compileRequiredArgument requiredArguments
            ]
                |> List.concat

        {- Instructions for the function body. -}
        instructionsForBody =
            [ List.concatMap compile body
            , [ PopLocalScope
              , Vm.Vm.Return
              ]
            ]
                |> List.concat

        numberOfRequiredArguments =
            List.length requiredArguments

        numberOfOptionalArguments =
            List.length optionalArguments

        {- Instructions if an optional argument is compiled like a required
           argument. In this case, the value for the argument is expected to be
           passed on the stack.
        -}
        instructionsRequired =
            List.map (Tuple.first >> compileRequiredArgument) optionalArguments

        {- Instructions if an optional argument is compiled as optional
           argument. In this case, the value for the argument is computed by the callee itself.
        -}
        instructionsOptional =
            List.map compileOptionalArgument optionalArguments

        {- Given a number of optional arguments to be compiled as required
           arguments, compile a function body.
        -}
        compileBody : Int -> List Instruction
        compileBody i =
            let
                instructions =
                    (List.take i instructionsRequired)
                        ++ (List.drop i instructionsOptional)
                        |> List.concat
            in
                [ instructionsForRequiredArguments
                , instructions
                , instructionsForBody
                ]
                    |> List.concat
    in
        {- Compile the function for each number of arguments. If, e. g., a
           function takes 1 required and 2 optional arguments, the list
           returned by this function will contain a function for each of the
           following cases:

             - 1 required argument, 2 optional arguments,
             - 2 required arguments, 1 optional argument,
             - 3 required arguments.
        -}
        List.map
            (\i ->
                { name = mangleName name (numberOfRequiredArguments + i)
                , body = compileBody i
                }
            )
            (List.range 0 numberOfOptionalArguments |> List.reverse)
