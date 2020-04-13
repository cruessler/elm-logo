module Compiler.Parser.Problem exposing (Problem(..))


type Problem
    = InvalidNumber
    | InvalidFunctionCall String
    | InvalidPrimitive String
    | ExpectingStartOfFunctionName
    | ExpectingSpace
    | ExpectingStartOfWord
    | ExpectingNumber
    | ExpectingWord
    | ExpectingDigit
    | ExpectingKeyword String
    | ExpectingOperator String
    | ExpectingSymbol String
    | ExpectingNewline
    | ExpectingEnd
