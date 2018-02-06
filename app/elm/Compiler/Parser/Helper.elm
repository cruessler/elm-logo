module Compiler.Parser.Helper exposing (list, repeatExactly)

import Parser
    exposing
        ( Parser
        , Count(..)
        , (|.)
        , (|=)
        , succeed
        , zeroOrMore
        , oneOf
        , andThen
        , delayedCommit
        , symbol
        , ignore
        )


list :
    { item : Parser a, separator : Parser () }
    -> Parser (List a)
list { item, separator } =
    let
        rest : a -> Parser (List a)
        rest first =
            Parser.repeat zeroOrMore
                (delayedCommit separator <| item)
                |> Parser.map (\rest -> first :: rest)
    in
        oneOf
            [ item |> andThen rest
            , succeed []
            ]


repeatExactly :
    Int
    -> { item : Parser a, separator : Parser () }
    -> Parser (List a)
repeatExactly count { item, separator } =
    let
        rest : a -> Parser (List a)
        rest first =
            Parser.repeat (Exactly (count - 1))
                (delayedCommit separator <| item)
                |> Parser.map (\rest -> first :: rest)
    in
        if count > 0 then
            item |> andThen rest
        else
            succeed []
