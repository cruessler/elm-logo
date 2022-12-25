module Test.Run.StandardLibrary exposing (functions)

import Test exposing (Test, describe)
import Test.Helper exposing (printsLine)


functions : Test
functions =
    describe "standard library" <|
        [ describe "combine" <|
            [ printsLine "print combine \"a \"b" "ab"
            , printsLine "print combine \"a [b c]" "a b c"
            , printsLine "print combine \"a [b [c]]" "a b [c]"
            , printsLine "print combine [a] [b c]" "[a] b c"
            ]
        ]
