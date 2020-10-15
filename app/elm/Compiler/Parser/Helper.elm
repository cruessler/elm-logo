module Compiler.Parser.Helper exposing
    ( functionName
    , keyword
    , list
    , maybeSpaces
    , operator
    , repeatAtMost
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


repeatAtMost :
    Int
    -> Config context problem a
    -> Parser context problem (List a)
repeatAtMost count config =
    let
        rest : a -> Parser context problem (List a)
        rest first =
            loop ( count - 1, [ first ] ) <| repeatAtMost_ config
    in
    if count > 0 then
        config.item |> andThen rest

    else
        succeed []


repeatAtMost_ :
    Config context problem a
    -> ( Int, List a )
    -> Parser context problem (Step ( Int, List a ) (List a))
repeatAtMost_ { item, separator } ( count, acc ) =
    let
        endLoop : Parser context problem (Step ( Int, List a ) (List a))
        endLoop =
            succeed ()
                |> map (\_ -> Done <| List.reverse acc)
    in
    if count > 0 then
        P.oneOf
            [ succeed (\next -> Loop ( count - 1, next :: acc ))
                |. backtrackable separator
                |= item
            , endLoop
            ]

    else
        endLoop


functionName : Parser context Problem String
functionName =
    let
        isStartCharacter =
            \c ->
                (c /= '[')
                    && (c /= ']')
                    && (c /= '(')
                    && (c /= ')')
                    && (c /= ':')
                    && (c /= '"')
                    && (c /= '?')
                    && (c /= '\n')
                    && (not <| Char.isDigit c)

        isRestCharacter =
            \c ->
                (c /= '[')
                    && (c /= ']')
                    && (c /= '(')
                    && (c /= ')')
                    && (c /= ' ')
                    && (c /= '\n')
    in
    getChompedString <|
        succeed ()
            |. chompIf isStartCharacter ExpectingStartOfFunctionName
            |. chompWhile isRestCharacter


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
