module Compiler.Ast exposing (Node(..), compile)

{-| This module provides types and functions for working with Logo ASTs.
-}

import Vm.Command as C
import Vm.Introspect as I
import Vm.Primitive as P
import Vm.Type as Type
import Vm.Vm exposing (Instruction(..))


type Node
    = Repeat Node (List Node)
    | Command1 C.Command1 Node
    | Primitive1 P.Primitive1 Node
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
                , [ Introspect0 { name = "repcount", f = I.repcount }
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

        Value value ->
            [ PushValue value ]
