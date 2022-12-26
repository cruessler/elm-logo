module Test.Ast exposing (..)

import Compiler.Ast as Ast
import Expect
import Test exposing (..)
import Vm.Command as C
import Vm.Exception as Exception
import Vm.Instruction exposing (Instruction(..))
import Vm.Primitive as P
import Vm.Type as Type


intLiteral : Ast.Node
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
                        emptyp |> Ast.compile (Ast.Expression { caller = "" })
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
                            |> Ast.compile Ast.Statement
                in
                Expect.equal ast
                    [ PushValue (Type.Int 10)
                    , Duplicate
                    , Eval1 { name = "integerp", f = P.integerp }
                    , JumpIfTrue 2
                    , Raise (Exception.WrongInput "repeat")
                    , PushLoopScope
                    , EnterLoopScope
                    , JumpIfTrue 5
                    , PushValue (Type.Word "")
                    , Eval1 { name = "emptyp", f = P.emptyp }
                    , Command1 { name = "print", f = C.print }
                    , Jump -5
                    , PopLoopScope
                    ]
        ]
