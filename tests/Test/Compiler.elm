module Test.Compiler exposing (..)

import Compiler.Ast as Ast
import Expect
import Test exposing (Test, describe, test)
import Vm.Instruction exposing (Instruction(..))
import Vm.Type as Type


compileFunction : Test
compileFunction =
    describe "compiles a function node" <|
        [ test "with one optional argument" <|
            \_ ->
                let
                    function =
                        { name = "foo"
                        , requiredArguments = [ "bar" ]
                        , optionalArguments = [ ( "baz", Ast.Value <| Type.Word "baz" ) ]
                        , body = []
                        }

                    compiledFunctions =
                        Ast.compileFunction function
                in
                Expect.equal
                    { name = "foo"
                    , requiredArguments = [ "bar" ]
                    , optionalArguments = [ "baz" ]
                    , instances =
                        [ { mangledName = "foo2"
                          , body =
                                [ PushLocalScope
                                , LocalVariable "bar"
                                , StoreVariable "bar"
                                , LocalVariable "baz"
                                , StoreVariable "baz"
                                , PushVoid
                                , PopLocalScope
                                , Return
                                ]
                          }
                        , { mangledName = "foo1"
                          , body =
                                [ PushLocalScope
                                , LocalVariable "bar"
                                , StoreVariable "bar"
                                , LocalVariable "baz"
                                , PushValue (Type.Word "baz")
                                , StoreVariable "baz"
                                , PushVoid
                                , PopLocalScope
                                , Return
                                ]
                          }
                        ]
                    }
                    compiledFunctions
        ]
