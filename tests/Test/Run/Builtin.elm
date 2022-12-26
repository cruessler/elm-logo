module Test.Run.Builtin exposing (commands, primitives, print, show)

import Test exposing (Test, describe)
import Test.Helper exposing (printsLine, runsWithoutError)


commands : Test
commands =
    describe "commands and their shorthands" <|
        [ runsWithoutError "print 1"
        , runsWithoutError "pr 1"
        , runsWithoutError "type 1"
        , runsWithoutError "forward 1"
        , runsWithoutError "fd 1"
        , runsWithoutError "back 1"
        , runsWithoutError "bk 1"
        , runsWithoutError "left 1"
        , runsWithoutError "lt 1"
        , runsWithoutError "right 1"
        , runsWithoutError "rt 1"
        , runsWithoutError "setxy 1 1"
        , runsWithoutError "pendown"
        , runsWithoutError "pd"
        , runsWithoutError "penup"
        , runsWithoutError "pu"
        , runsWithoutError "setpencolor 1"
        , runsWithoutError "setpc 1"
        , runsWithoutError "home"
        , runsWithoutError "clean"
        , runsWithoutError "clearscreen"
        , runsWithoutError "cs"
        ]


primitives : Test
primitives =
    describe "primitives and their shorthands" <|
        [ describe "butfirst" <|
            [ printsLine "print butfirst [ 1 1 ]" "1"
            , printsLine "print bf [ 1 1 ]" "1"
            ]
        , describe "emptyp" <|
            [ printsLine "print emptyp []" "true"
            , printsLine "print empty? []" "true"
            ]
        , describe "first" <|
            [ printsLine "print first [ 1 ]" "1" ]
        , describe "integerp" <|
            [ printsLine "print integerp 1" "true" ]
        , describe "floatp" <|
            [ printsLine "print floatp 1.0" "true" ]
        , describe "wordp" <|
            [ printsLine "print wordp \"word" "true"
            , printsLine "print wordp 1" "true"
            , printsLine "print wordp 1.0" "true"
            , printsLine "print wordp []" "false"
            ]
        , describe "equalp" <|
            [ printsLine "print equalp 1 1" "true"
            , printsLine "print equal? 1 1" "true"
            ]
        , describe "notequalp" <|
            [ printsLine "print notequalp 1 1" "false"
            , printsLine "print notequal? 1 1" "false"
            ]
        , describe "minus" <|
            [ printsLine "print minus 1" "-1" ]
        , describe "sum" <|
            [ printsLine "print sum 1 1" "2"
            , printsLine "print (sum 1 2 3)" "6"
            , printsLine "print equalp (sum 1 2 3) 1 + 2 + 3" "true"
            ]
        , describe "difference" <|
            [ printsLine "print difference 1 1" "0" ]
        , describe "product" <|
            [ printsLine "print product 1 1" "1" ]
        , describe "quotient" <|
            [ printsLine "print quotient 1 1" "1" ]
        , describe "round" <|
            [ printsLine "print round 1.2" "1"
            , printsLine "print round 1.5" "2"
            , printsLine "print round minus 1.5" "-2"
            , printsLine "print round minus 1.8" "-2"
            ]
        , describe "greaterp" <|
            [ printsLine "print greaterp 1 1" "false"
            , printsLine "print greater? 1 1" "false"
            ]
        , describe "lessp" <|
            [ printsLine "print lessp 1 1" "false"
            , printsLine "print less? 1 1" "false"
            ]
        , describe "remainder" <|
            [ printsLine "print remainder 1 1" "0" ]
        , describe "sentence" <|
            [ printsLine "print sentence 1 1" "1 1"
            , printsLine "print se 1 1" "1 1"
            ]
        , describe "word" <|
            [ printsLine "print word 1 1" "11" ]
        , describe "char" <|
            [ printsLine "print char 65" "A" ]
        , describe "bitand" <|
            [ printsLine "print (bitand 7 5 4)" "4"
            , printsLine "print bitand 7 minus 4" "4"
            ]
        , describe "bitnot" <|
            [ printsLine "print bitnot 6" "-7" ]
        , describe "ashift" <|
            [ printsLine "print ashift 1 3" "8"
            , printsLine "print ashift 8 minus 1" "4"
            ]
        , describe "lshift" <|
            [ printsLine "print lshift 1 3" "8"
            , printsLine "print lshift 1 minus 2" "0"
            , printsLine "print lshift 8 minus 1" "4"
            , printsLine "print lshift 1 minus 32" "1"
            ]
        , describe "fput" <|
            [ printsLine "print fput \"w \"ord" "word"
            , printsLine "print fput \"w [ o r d ]" "w o r d"
            ]
        ]


print : Test
print =
    describe "print" <|
        [ printsLine "(print 1 2)" "1 2"
        , printsLine
            "(print [Move disk from] \"from [to] \"to)"
            "Move disk from from to to"
        ]


show : Test
show =
    describe "show" <|
        [ printsLine "show [ 1 1 ]" "[1 1]"
        , printsLine "show [ 1 [ 2 ] ]" "[1 [2]]"
        , printsLine "show 1" "1"
        , printsLine "show \"word" "word"
        , printsLine "show sentence 1 1" "[1 1]"
        ]
