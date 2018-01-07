module Test.Ast exposing (..)

import Compiler.Ast as Ast
import Expect exposing (Expectation)
import Test exposing (..)
import Vm.Command as C
import Vm.Introspect as I
import Vm.Primitive as P
import Vm.Type as Type
import Vm.Vm exposing (Instruction(..))


intLiteral =
    Ast.Value (Type.Int 10)


emptyp : Ast.Node
emptyp =
    Ast.Primitive1
        { name = "emptyp", f = P.emptyp }
        (Ast.Value <| Type.Word "")


print : Ast.Node -> Ast.Node
print node =
    Ast.Command1
        { name = "print", f = C.print }
        node


primitive : Test
primitive =
    describe "code generation for primitives" <|
        [ test "primitive with one argument" <|
            \_ ->
                let
                    ast =
                        emptyp |> Ast.compile
                in
                    Expect.equal ast
                        [ PushValue (Type.Word "")
                        , Eval1 { name = "emptyp", f = P.emptyp }
                        ]
        ]


repeat : Test
repeat =
    describe "code generation for repeat" <|
        [ test "with static value" <|
            \_ ->
                let
                    ast =
                        Ast.Repeat intLiteral [ print emptyp ]
                            |> Ast.compile
                in
                    Expect.equal ast
                        [ PushLoopScope
                        , PushValue (Type.Int 10)
                        , Introspect0 { name = "repcount", f = I.repcount }
                        , Eval2 { name = "lessThan", f = P.lessThan }
                        , JumpIfFalse 6
                        , EnterLoopScope
                        , PushValue (Type.Word "")
                        , Eval1 { name = "emptyp", f = P.emptyp }
                        , Command1 { name = "print", f = C.print }
                        , Jump -8
                        , PopLoopScope
                        ]
        ]
