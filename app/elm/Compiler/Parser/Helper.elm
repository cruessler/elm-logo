module Compiler.Parser.Helper exposing
    ( functionName
    , keyword
    , list
    , maybeSpaces
    , operator
    , repeatExactly
    , spaces
    , symbol
    )

import Char
import Compiler.Parser.Problem exposing (Problem(..))
import Parser.Advanced as P
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , andThen
        , backtrackable
        , chompIf
        , chompWhile
        , getChompedString
        , loop
        , map
        , oneOf
        , succeed
        )


type alias Config context problem a =
    { item : Parser context problem a
    , separator : Parser context problem ()
    }


list :
    Config context problem a
    -> Parser context problem (List a)
list config =
    let
        rest : a -> Parser context problem (List a)
        rest first =
            loop [ first ] (list_ config)
    in
    oneOf
        [ config.item |> andThen rest
        , succeed []
        ]


list_ :
    Config context problem a
    -> List a
    -> Parser context problem (Step (List a) (List a))
list_ { item, separator } acc =
    oneOf
        [ succeed (\next -> Loop (next :: acc))
            |. backtrackable separator
            |= item
        , succeed ()
            |> map (\_ -> Done <| List.reverse acc)
        ]


repeatExactly :
    Int
    -> Config context problem a
    -> Parser context problem (List a)
repeatExactly count config =
    let
        rest : a -> Parser context problem (List a)
        rest first =
            loop ( count - 1, [ first ] ) <| repeatExactly_ config
    in
    if count > 0 then
        config.item |> andThen rest

    else
        succeed []


repeatExactly_ :
    Config context problem a
    -> ( Int, List a )
    -> Parser context problem (Step ( Int, List a ) (List a))
repeatExactly_ { item, separator } ( count, acc ) =
    if count > 0 then
        succeed (\next -> Loop ( count - 1, next :: acc ))
            |. separator
            |= item

    else
        succeed ()
            |> map (\_ -> Done <| List.reverse acc)


functionName : Parser context Problem String
functionName =
    getChompedString <|
        succeed ()
            |. chompIf
                (\c ->
                    (c /= '[')
                        && (c /= ']')
                        && (c /= '(')
                        && (c /= ')')
                        && (c /= ':')
                        && (c /= '"')
                        && (c /= '?')
                        && (c /= '\n')
                        && (not <| Char.isDigit c)
                )
                ExpectingStartOfFunctionName
            |. chompWhile (\c -> c /= ' ' && c /= '\n')


maybeSpaces : Parser context problem ()
maybeSpaces =
    chompWhile (\c -> c == ' ')


keyword : String -> Parser context Problem ()
keyword keyword_ =
    P.keyword (P.Token keyword_ <| ExpectingKeyword keyword_)


operator : String -> Parser context Problem ()
operator operator_ =
    P.symbol (P.Token operator_ <| ExpectingOperator operator_)


symbol : String -> Parser context Problem ()
symbol symbol_ =
    P.symbol (P.Token symbol_ <| ExpectingSymbol symbol_)


spaces : Parser context Problem ()
spaces =
    succeed ()
        |. chompIf (\c -> c == ' ') ExpectingSpace
        |. chompWhile (\c -> c == ' ')
