module Test.Compiler exposing (..)

import Compiler.Ast as Ast exposing (Node(..))
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import Vm.Type as Type
import Vm.Vm as Vm exposing (Instruction(..))


compileFunction : Test
compileFunction =
    describe "compiles a function node" <|
        [ test "with one optional argument" <|
            \_ ->
                let
                    function =
                        { name = "foo"
                        , requiredArguments = [ "bar" ]
                        , optionalArguments = [ ( "baz", Value <| Type.Word "baz" ) ]
                        , body = []
                        }

                    compiledFunctions =
                        Ast.compileFunction function
                in
                    Expect.equal
                        [ { name = "foo2"
                          , body =
                                [ PushLocalScope
                                , LocalVariable "bar"
                                , StoreVariable "bar"
                                , LocalVariable "baz"
                                , StoreVariable "baz"
                                , PopLocalScope
                                , Vm.Return
                                ]
                          }
                        , { name = "foo1"
                          , body =
                                [ PushLocalScope
                                , LocalVariable "bar"
                                , StoreVariable "bar"
                                , LocalVariable "baz"
                                , PushValue (Type.Word "baz")
                                , StoreVariable "baz"
                                , PopLocalScope
                                , Vm.Return
                                ]
                          }
                        ]
                        compiledFunctions
        ]
