module Test.Encode exposing (compilePrograms, encodeAndDecodeScope, encodeAndDecodeStack, encodeAndDecodeVms)

import Expect
import Json.Decode as D
import Json.Encode as E
import Logo
import Test exposing (Test, describe, test)
import Ui.Machine as Machine
import Ui.Machine.Scope as Scope
import Vm.Scope as Scope
import Vm.Stack as Stack
import Vm.Type as Type
import Vm.Vm as Vm


programs : List String
programs =
    [ """to snowflake :length :depth
if :depth = 0 [ forward :length ]
if :depth <> 0 [ snowflake :length / 3 :depth - 1 right 60 snowflake :length / 3 :depth - 1 left 120 snowflake :length / 3 :depth - 1 right 60 snowflake :length / 3 :depth - 1 ]
end
snowflake 350 3
"""
    , """to square :length
repeat 4 [ forward :length right 90 ]
end
pendown square 100
"""
    , """to choices :menu [:sofar []]
if emptyp :menu [print :sofar stop]
foreach first :menu [(choices butfirst :menu sentence :sofar ?)]
end
choices [[small medium large] [vanilla [ultra chocolate] lychee [rum raisin] ginger] [cone cup]]
"""
    ]


encodeAndDecodeVm : String -> Test
encodeAndDecodeVm program =
    let
        decodedMachine =
            Logo.empty
                |> Logo.compile program
                |> Logo.getVm
                |> Vm.toValue
                |> Machine.fromValue

        name =
            "encodes and decodes VM for " ++ program
    in
    test name <|
        \_ ->
            Expect.ok decodedMachine


encodeAndDecodeVms : Test
encodeAndDecodeVms =
    Test.concat (List.map encodeAndDecodeVm programs)


compileProgram : String -> Test
compileProgram program =
    let
        logo =
            Logo.compile program Logo.empty

        name =
            "compiles " ++ program
    in
    test name <|
        \_ ->
            if Logo.done logo then
                Expect.fail <| "failed to compile " ++ program

            else
                Expect.pass


compilePrograms : Test
compilePrograms =
    Test.concat (List.map compileProgram programs)


word : Type.Value
word =
    Type.Word "word"


list : Type.Value
list =
    Type.List [ word, Type.List [ word ] ]


encodeAndDecodeScope : Test
encodeAndDecodeScope =
    describe "Scope" <|
        [ test "encodes and decodes scopes" <|
            \_ ->
                let
                    scope =
                        Scope.empty
                            |> Scope.make "x" word
                            |> Scope.pushLocalScope 10
                            |> Scope.make "x" word
                            |> Scope.make "y" word
                            |> Scope.pushTemplateScope word
                            |> Scope.pushLoopScope 10

                    value =
                        E.list Scope.toValue scope
                in
                D.decodeValue (D.list Scope.scope) value
                    |> Expect.ok
        ]


encodeAndDecodeStack : Test
encodeAndDecodeStack =
    describe "Stack" <|
        [ test "encodes and decodes stack values" <|
            \_ ->
                let
                    stack =
                        [ Stack.Void, Stack.Address 10, Stack.Value word, Stack.Value list ]

                    value =
                        Stack.toValue stack
                in
                D.decodeValue (D.list D.string) value
                    |> Expect.ok
        ]
