module Compiler.Parser.Problem exposing (Problem(..), toString)


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
    | TooManyInputs String
    | NotEnoughInputs String


toString : Problem -> String
toString problem =
    case problem of
        NotEnoughInputs callable ->
            "not enough inputs to " ++ callable

        TooManyInputs callable ->
            "too many inputs to " ++ callable

        _ ->
            Debug.todo <| "toString not implemented for " ++ Debug.toString problem
