module Compiler.Parser exposing (value)

{-| This module provides functions for parsing Logo code.
-}

import Compiler.Ast as Ast
import Parser
    exposing
        ( Parser
        , (|.)
        , (|=)
        , succeed
        , zeroOrMore
        , oneOrMore
        , oneOf
        , andThen
        , delayedCommit
        , symbol
        , keep
        , ignore
        , lazy
        )
import Vm.Type as Type


value : Parser Ast.Node
value =
    Parser.map Ast.Value value_


value_ : Parser Type.Value
value_ =
    oneOf
        [ lazy (\_ -> list)
        , int
        , word
        ]


list : Parser Type.Value
list =
    succeed Type.List
        |. symbol "["
        |. maybeSpaces
        |= (value_ |> andThen (\value -> nextValueInList [ value ]))
        |. maybeSpaces
        |. symbol "]"


nextValueInList : List Type.Value -> Parser (List Type.Value)
nextValueInList acc =
    oneOf
        [ nextValueInList_ |> andThen (\value -> nextValueInList (value :: acc))
        , succeed (List.reverse acc)
        ]


nextValueInList_ : Parser Type.Value
nextValueInList_ =
    delayedCommit spaces <|
        succeed identity
            |= value_


int : Parser Type.Value
int =
    Parser.int
        |> Parser.map Type.Int


word : Parser Type.Value
word =
    succeed Type.Word
        |. symbol "\""
        |= keep oneOrMore (\c -> c /= ' ')


maybeSpaces : Parser ()
maybeSpaces =
    ignore zeroOrMore (\c -> c == ' ')


spaces : Parser ()
spaces =
    ignore oneOrMore (\c -> c == ' ')
