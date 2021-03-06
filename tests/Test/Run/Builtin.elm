module Test.Run.Builtin exposing (commands, primitives)

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
        [ printsLine "print butfirst [ 1 1 ]" "1"
        , printsLine "print bf [ 1 1 ]" "1"
        , printsLine "print emptyp []" "true"
        , printsLine "print empty? []" "true"
        , printsLine "print first [ 1 ]" "1"
        , printsLine "print integerp 1" "true"
        , printsLine "print floatp 1.0" "true"
        , printsLine "print equalp 1 1" "true"
        , printsLine "print equal? 1 1" "true"
        , printsLine "print notequalp 1 1" "false"
        , printsLine "print notequal? 1 1" "false"
        , printsLine "print minus 1" "-1"
        , printsLine "print sum 1 1" "2"
        , printsLine "print difference 1 1" "0"
        , printsLine "print product 1 1" "1"
        , printsLine "print quotient 1 1" "1"
        , printsLine "print greaterp 1 1" "false"
        , printsLine "print greater? 1 1" "false"
        , printsLine "print lessp 1 1" "false"
        , printsLine "print less? 1 1" "false"
        , printsLine "print remainder 1 1" "0"
        , printsLine "print sentence 1 1" "1 1"
        , printsLine "print se 1 1" "1 1"
        , printsLine "print word 1 1" "11"
        , printsLine "print char 65" "A"
        ]
