module Compiler.Parser.Value
    exposing
        ( value
        , wordOutsideList
        )

{-| This module provides functions for parsing Logo values.
-}

import Compiler.Ast as Ast
import Compiler.Parser.Helper as Helper
import Parser
    exposing
        ( Parser
        , Count(..)
        , (|.)
        , (|=)
        , inContext
        , lazy
        , oneOf
        , succeed
        , zeroOrMore
        , oneOrMore
        , keep
        , ignore
        , symbol
        )
import Vm.Type as Type


value : Parser Ast.Node
value =
    Parser.map Ast.Value valueOutsideList


valueOutsideList : Parser Type.Value
valueOutsideList =
    inContext "valueOutsideList" <|
        oneOf
            [ lazy (\_ -> list)
            , number
            , wordOutsideList
            ]


number : Parser Type.Value
number =
    Parser.float
        |> Parser.sourceMap
            (\source float ->
                case String.toInt source of
                    Ok int ->
                        Type.Int int

                    _ ->
                        Type.Word <| toString float
            )


wordOutsideList : Parser Type.Value
wordOutsideList =
    succeed Type.Word
        |. symbol "\""
        |= keep oneOrMore
            (\c ->
                (c /= ' ')
                    && (c /= '[')
                    && (c /= ']')
                    && (c /= '(')
                    && (c /= ')')
                    && (c /= '\n')
            )


list : Parser Type.Value
list =
    succeed Type.List
        |. symbol "["
        |. Helper.maybeSpaces
        |= Helper.list { item = valueInList, separator = Helper.spaces }
        |. Helper.maybeSpaces
        |. symbol "]"


valueInList : Parser Type.Value
valueInList =
    inContext "valueInList" <|
        oneOf
            [ lazy (\_ -> list)
            , number
            , wordInList
            ]


wordInList : Parser Type.Value
wordInList =
    succeed Type.Word
        |= keep oneOrMore
            (\c ->
                (c /= ' ')
                    && (c /= '[')
                    && (c /= ']')
                    && (c /= '(')
                    && (c /= ')')
                    && (c /= '\n')
                    && (c /= '+')
                    && (c /= '-')
                    && (c /= '*')
                    && (c /= '/')
                    && (c /= '=')
                    && (c /= '<')
                    && (c /= '>')
            )
