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
        , describe "queue" <|
            [ printsLine "make \"q [] queue \"q 1 queue \"q [2] print :q" "1 [2]"
            ]
        , describe "reverse" <|
            [ printsLine "print reverse [a b c d]" "d c b a"
            , printsLine "print reverse \"word" "drow"
            , printsLine "show reverse []" "[]"
            , printsLine "show reverse \"" "||"
            ]
        ]
