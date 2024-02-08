module Compiler.Parser.Value exposing
    ( rawWordInList
    , value
    )

{-| This module provides functions for parsing Logo values.
-}

import Compiler.Ast as Ast
import Compiler.Parser.Context exposing (Context(..))
import Compiler.Parser.Helper as Helper
import Compiler.Parser.Problem exposing (Problem(..))
import Parser.Advanced as P
    exposing
        ( (|.)
        , (|=)
        , Parser
        , chompIf
        , chompWhile
        , getChompedString
        , lazy
        , map
        , oneOf
        , succeed
        )
import Vm.Type as Type


value : Parser Context Problem Ast.Node
value =
    P.map Ast.Value valueOutsideList


valueOutsideList : Parser Context Problem Type.Value
valueOutsideList =
    P.inContext ValueOutsideList <|
        oneOf
            [ lazy (\_ -> list)
            , number
            , wordInVerticalBars
            , wordOutsideList
            ]


number : Parser context Problem Type.Value
number =
    P.number
        { int = Ok Type.Int
        , float = Ok (Type.Word << String.fromFloat)
        , hex = Err InvalidNumber
        , octal = Err InvalidNumber
        , binary = Err InvalidNumber
        , invalid = InvalidNumber
        , expecting = ExpectingNumber
        }


wordInVerticalBars : Parser context Problem Type.Value
wordInVerticalBars =
    let
        word : Parser context Problem String
        word =
            succeed ()
                |. chompWhile
                    (\c ->
                        (c /= '|')
                            && (c /= '\n')
                    )
                |> getChompedString
    in
    succeed Type.Word
        |. Helper.symbol "\"|"
        |= word
        |. Helper.symbol "|"


wordOutsideList : Parser context Problem Type.Value
wordOutsideList =
    let
        word : Parser context Problem String
        word =
            succeed ()
                |. chompWhile
                    (\c ->
                        (c /= ' ')
                            && (c /= '[')
                            && (c /= ']')
                            && (c /= '(')
                            && (c /= ')')
                            && (c /= '\n')
                    )
                |> getChompedString
    in
    succeed Type.Word
        |. Helper.symbol "\""
        |= word


list : Parser Context Problem Type.Value
list =
    succeed Type.List
        |. Helper.symbol "["
        |. Helper.maybeSpaces
        |= Helper.list { item = valueInList, separator = Helper.spaces }
        |. Helper.maybeSpaces
        |. Helper.symbol "]"


valueInList : Parser Context Problem Type.Value
valueInList =
    P.inContext ValueInList <|
        oneOf
            [ lazy (\_ -> list)
            , number
            , wordInList
            ]


rawWordInList : Parser context Problem String
rawWordInList =
    let
        isWordCharacter : Char -> Bool
        isWordCharacter c =
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
    in
    (succeed ()
        |. chompIf isWordCharacter ExpectingStartOfWord
        |. chompWhile isWordCharacter
    )
        |> getChompedString


wordInList : Parser context Problem Type.Value
wordInList =
    rawWordInList
        |> map Type.Word
