module Test.Type exposing (toDebugString, toString)

import Expect
import Test exposing (Test, describe, test)
import Vm.Type as T


toString : Test
toString =
    describe "toString" <|
        [ test "nested list" <|
            \_ ->
                let
                    list =
                        T.List
                            [ T.List
                                [ T.Word "small"
                                , T.Word "medium"
                                , T.Word "large"
                                ]
                            , T.List
                                [ T.Word "vanilla"
                                , T.List [ T.Word "ultra", T.Word "chocolate" ]
                                , T.Word "lychee"
                                , T.List [ T.Word "rum", T.Word "raisin" ]
                                , T.Word "ginger"
                                ]
                            , T.List
                                [ T.Word "cone"
                                , T.Word "cup"
                                ]
                            ]
                in
                Expect.equal
                    "[small medium large] [vanilla [ultra chocolate] lychee [rum raisin] ginger] [cone cup]"
                    (T.toString list)
        ]


toDebugString : Test
toDebugString =
    describe "toDebugString" <|
        [ test "nested list" <|
            \_ ->
                let
                    list =
                        T.List
                            [ T.List
                                [ T.Word "small"
                                ]
                            , T.List
                                [ T.Word "vanilla"
                                ]
                            ]
                in
                Expect.equal
                    "[[small] [vanilla]]"
                    (T.toDebugString list)
        ]
