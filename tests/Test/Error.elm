module Test.Error exposing (..)

import Environment.History exposing (Entry(..))
import Expect
import Logo
import Test exposing (Test, describe, test)


printsError : String -> String -> Test
printsError program expectedMessage =
    test program <|
        \_ ->
            let
                logo =
                    Logo.run program Logo.empty

                history =
                    Logo.getHistory logo

                last =
                    List.head history
            in
            case last of
                Just (Error actualMessage) ->
                    Expect.equal expectedMessage actualMessage

                Just _ ->
                    Expect.fail <|
                        "expected error message \""
                            ++ expectedMessage
                            ++ "\", but last history entry was not an error message"

                Nothing ->
                    Expect.fail <|
                        "expected error message \""
                            ++ expectedMessage
                            ++ "\", but history was empty"


repeatWithInvalidArguments : Test
repeatWithInvalidArguments =
    describe "repeat fails given non-integer argument" <|
        [ printsError "repeat \"foo []" "repeat doesn’t like foo as input"
        ]


functionsWithInvalidArguments : Test
functionsWithInvalidArguments =
    describe "functions fail given argument of wrong type" <|
        [ printsError "print butfirst []" "butfirst doesn’t like [] as input"
        , printsError "print first []" "first doesn’t like [] as input"
        , printsError "print first butfirst \"a" "first doesn’t like || as input"
        , printsError "first butfirst \"a" "first doesn’t like || as input"
        , printsError "print lessp \"word \"word" "lessp doesn’t like word as input"
        , printsError "print lessp \"word []" "lessp doesn’t like word as input"
        , printsError "if \"word [ print \"word ]" "if doesn’t like word as input"
        , printsError "print 1 > []" "> doesn’t like [] as input"
        , printsError "print greaterp 1 []" "greaterp doesn’t like [] as input"
        , printsError "print 1 + []" "+ doesn’t like [] as input"
        , printsError "print sum 1 []" "sum doesn’t like [] as input"
        , printsError "print 1 - []" "- doesn’t like [] as input"
        , printsError "print difference 1 []" "difference doesn’t like [] as input"
        , printsError "print 1 * []" "* doesn’t like [] as input"
        , printsError "print product 1 []" "product doesn’t like [] as input"
        , printsError "print 1 / []" "/ doesn’t like [] as input"
        , printsError "print quotient 1 []" "quotient doesn’t like [] as input"
        , printsError "print minus []" "minus doesn’t like [] as input"
        ]


noOutput : Test
noOutput =
    describe "prints error if no value is returned" <|
        [ printsError """to foo :bar
print "bar
end
print foo "baz""" "foo did not output to print"
        , printsError """to foo :bar
repeat 4 [ print 4 ]
end
print foo "baz""" "foo did not output to print"
        , printsError "print print 3" "print did not output to print"
        , printsError "print print print 3" "print did not output to print"
        , printsError "if print 3 [ print 3 ]" "print did not output to if"
        , printsError "print if \"true []" "if did not output to print"
        , printsError "print repeat 1 [ print 1 ]" "repeat did not output to print"
        , printsError "print foreach 1 [ print 1 ]" "foreach did not output to print"
        , printsError """print make "foo "bar""" "make did not output to print"
        , printsError "sum print 3 print 3" "print did not output to sum"
        , printsError "sum 3 print 3" "print did not output to sum"
        , printsError "print if 1 = 1 [ print 1 ]" "print did not output to print"
        , printsError "minus sum 3 print 3" "print did not output to sum"
        , printsError "1 + print 3" "print did not output to +"
        , printsError "1 = print 3" "print did not output to ="
        , printsError "1 <> print 3" "print did not output to <>"
        , printsError "1 > print 3" "print did not output to >"
        , printsError "print ifelse \"true [] []" "ifelse did not output to print"
        , printsError "print ifelse \"false [] []" "ifelse did not output to print"
        ]


outputOutsideProcedure : Test
outputOutsideProcedure =
    describe "prints error if output is used outside procedure" <|
        [ printsError """to foo :bar [:baz output 5]
print :bar
end
foo "bar""" "Can only use output inside a procedure"
        , printsError "output 5" "Can only use output inside a procedure"
        ]


noUseOfValue : Test
noUseOfValue =
    describe "prints error if value is not passed to function" <|
        [ printsError """to foo :bar
output "bar
end
foo "baz"""
            "You don’t say what to do with bar"
        , printsError "3" "You don’t say what to do with 3"
        , printsError "repcount" "You don’t say what to do with -1"
        , printsError "foreach 1 [ 5 ]" "You don’t say what to do with 5"
        , printsError "repeat 1 [ 5 ]" "You don’t say what to do with 5"
        , printsError "if \"true [ 5 ]" "You don’t say what to do with 5"
        , printsError "3 + 3" "You don’t say what to do with 6"
        , printsError "3 * 3 + 3" "You don’t say what to do with 12"
        , printsError "minus 3" "You don’t say what to do with -3"
        , printsError "sum 3 3" "You don’t say what to do with 6"
        , printsError "if 1 = 1 [ minus 1 ]" "You don’t say what to do with -1"
        , printsError "ifelse \"true [ 5 ] [ 6 ]" "You don’t say what to do with 5"
        , printsError "butfirst [ 1 ]" "You don’t say what to do with []"
        ]


undefinedVariable : Test
undefinedVariable =
    describe "prints error if undefined variable is used" <|
        [ printsError "print :variable" "variable has no value"
        , printsError ":variable" "variable has no value"
        ]


notEnoughInputs : Test
notEnoughInputs =
    describe "prints error if function is called with too few arguments" <|
        [ printsError
            """to foo :bar :baz
print :bar
end
(foo "bar)"""
            "not enough inputs to foo"
        , printsError "print" "not enough inputs to print"
        , printsError "   sum 3" "not enough inputs to sum"
        , printsError "minus   " "not enough inputs to minus"
        , printsError "sum print 3" "not enough inputs to sum"
        , printsError "difference print 3  " "not enough inputs to difference"
        ]


tooManyInputs : Test
tooManyInputs =
    describe "prints error if function is called with too many arguments" <|
        [ printsError
            """to foo :bar
print :bar
end
(foo "bar "baz)"""
            "too many inputs to foo"
        ]
