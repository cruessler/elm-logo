module Compiler.Parser
    exposing
        ( root
        , value
        , functionDefinition
        )

{-| This module provides functions for parsing Logo code.
-}

import Char
import Compiler.Ast as Ast
import Compiler.Ast.Command as Command
import Compiler.Ast.Introspect as Introspect
import Compiler.Ast.Primitive as Primitive
import Compiler.Parser.Helper as Parser
import Parser
    exposing
        ( Parser
        , Count(..)
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


root : Parser Ast.Program
root =
    Parser.inContext "root" <|
        succeed Ast.Program
            |= Parser.repeat zeroOrMore functionDefinition
            |= statements


functionDefinition : Parser Ast.Function
functionDefinition =
    Parser.inContext "functionDefinition" <|
        delayedCommit (Parser.keyword "to") <|
            (succeed Ast.Function
                |. spaces
                |= functionName
                |. spaces
                |= arguments
                |. maybeSpaces
                |. symbol "\n"
                |= functionBody
            )


functionName : Parser String
functionName =
    keep oneOrMore (\c -> c /= ' ')


argument : Parser String
argument =
    succeed identity
        |. symbol ":"
        |= keep oneOrMore (\c -> c /= ' ' && c /= '\n')


arguments : Parser (List String)
arguments =
    Parser.list { item = argument, separator = spaces }


functionBody : Parser (List Ast.Node)
functionBody =
    Parser.inContext "functionBody" <|
        (nextLine [])


nextLine : List Ast.Node -> Parser (List Ast.Node)
nextLine acc =
    oneOf
        [ Parser.keyword "end\n"
            |> andThen (\_ -> succeed acc)
        , line
            |> andThen (\line -> nextLine (List.concat [ acc, line ]))
        ]


line : Parser (List Ast.Node)
line =
    Parser.inContext "line" <|
        succeed identity
            |. maybeSpaces
            |= statements
            |. maybeSpaces
            |. symbol "\n"


statements : Parser (List Ast.Node)
statements =
    Parser.list { item = lazy (\_ -> statement), separator = spaces }


statement : Parser Ast.Node
statement =
    Parser.inContext "statement" <|
        oneOf
            [ lazy (\_ -> foreach)
            , lazy (\_ -> repeat)
            , lazy (\_ -> if_)
            , make
            , command
            ]


controlStructure : String -> (Ast.Node -> List Ast.Node -> Ast.Node) -> Parser Ast.Node
controlStructure keyword constructor =
    Parser.inContext keyword <|
        succeed constructor
            |. Parser.keyword keyword
            |. symbol " "
            |= expression
            |. spaces
            |. symbol "["
            |. maybeSpaces
            |= statements
            |. maybeSpaces
            |. symbol "]"


if_ : Parser Ast.Node
if_ =
    controlStructure "if" Ast.If


foreach : Parser Ast.Node
foreach =
    lazy (\_ -> controlStructure "foreach" Ast.Foreach)


repeat : Parser Ast.Node
repeat =
    controlStructure "repeat" Ast.Repeat


command : Parser Ast.Node
command =
    let
        command_ : String -> Parser Ast.Node
        command_ name =
            Command.find name
                |> Maybe.map commandWithArguments
                |> Maybe.withDefault
                    (Parser.fail <| "call to non-existent function `" ++ name ++ "`")
    in
        Parser.inContext "command" <|
            (call |> andThen command_)


commandWithArguments : Command.Command -> Parser Ast.Node
commandWithArguments command =
    let
        numberOfArguments =
            Command.arguments command

        makeNode : List Ast.Node -> Parser Ast.Node
        makeNode result =
            case ( command, result ) of
                ( Command.Command1 command1, [ first ] ) ->
                    succeed <| Ast.Command1 command1 first

                _ ->
                    Parser.fail "could not parse function call"
    in
        expressions numberOfArguments
            |> andThen makeNode


expressions : Int -> Parser (List Ast.Node)
expressions count =
    Parser.repeatExactly count { item = expression, separator = spaces }


expression : Parser Ast.Node
expression =
    Parser.inContext "expression" <|
        delayedCommit maybeSpaces <|
            oneOf
                [ templateVariable
                , variable
                , lazy (\_ -> primitive)
                , value
                ]


primitive : Parser Ast.Node
primitive =
    Parser.inContext "primitive" <|
        (call
            |> andThen (\name -> lazy (\_ -> primitive_ name))
        )


primitive_ : String -> Parser Ast.Node
primitive_ name =
    let
        primitive =
            Primitive.find name

        introspect =
            Introspect.find name
    in
        case ( primitive, introspect ) of
            ( Just primitive, _ ) ->
                primitiveWithArguments primitive

            ( _, Just introspect ) ->
                introspectWithArguments introspect

            _ ->
                Parser.fail "could not parse function call"


introspectWithArguments : Introspect.Introspect -> Parser Ast.Node
introspectWithArguments introspect =
    let
        numberOfArguments =
            Introspect.arguments introspect

        makeNode : List Ast.Node -> Parser Ast.Node
        makeNode result =
            case ( introspect, result ) of
                ( Introspect.Introspect1 introspect1, [ first ] ) ->
                    succeed <| Ast.Introspect1 introspect1 first

                ( Introspect.Introspect0 introspect0, [] ) ->
                    succeed <| Ast.Introspect0 introspect0

                _ ->
                    Parser.fail "could not parse function call"
    in
        if numberOfArguments == 0 then
            makeNode []
        else
            expressions numberOfArguments
                |> andThen makeNode


primitiveWithArguments : Primitive.Primitive -> Parser Ast.Node
primitiveWithArguments primitive =
    let
        numberOfArguments =
            Primitive.arguments primitive

        makeNode : List Ast.Node -> Parser Ast.Node
        makeNode result =
            case ( primitive, result ) of
                ( Primitive.Primitive2 primitive2, [ first, second ] ) ->
                    succeed <| Ast.Primitive2 primitive2 first second

                ( Primitive.Primitive1 primitive1, [ first ] ) ->
                    succeed <| Ast.Primitive1 primitive1 first

                _ ->
                    Parser.fail "could not parse function call"
    in
        expressions numberOfArguments
            |> andThen makeNode


templateVariable : Parser Ast.Node
templateVariable =
    let
        digits =
            keep oneOrMore Char.isDigit

        makeNode : String -> Ast.Node
        makeNode argument =
            Ast.Introspect1
                Introspect.templateVariable
                (Ast.Value <| Type.Word argument)
    in
        Parser.inContext "templateVariable" <|
            succeed makeNode
                |. symbol "?"
                |. maybeSpaces
                |= oneOf [ Parser.source <| Parser.keyword "rest", digits ]


call : Parser String
call =
    Parser.source <|
        ignore (Exactly 1)
            (\c ->
                (c /= '[')
                    && (c /= ']')
                    && (c /= '"')
                    && (c /= '\n')
                    && (not <| Char.isDigit c)
            )
            |. ignore zeroOrMore (\c -> c /= ' ' && c /= '\n')


make : Parser Ast.Node
make =
    let
        makeNode : ( Type.Value, Ast.Node ) -> Parser Ast.Node
        makeNode ( name, node ) =
            case name of
                Type.Word name ->
                    succeed <| Ast.Make name node

                _ ->
                    Parser.fail "make expects the first argument to be a word"
    in
        Parser.inContext "make" <|
            (succeed (,)
                |. Parser.keyword "make"
                |. spaces
                |= word
                |. spaces
                |= expression
                |> andThen makeNode
            )


variable : Parser Ast.Node
variable =
    Parser.inContext "variable" <|
        succeed Ast.Variable
            |. symbol ":"
            |= keep oneOrMore (\c -> c /= ' ' && c /= '\n')


value : Parser Ast.Node
value =
    Parser.map Ast.Value value_


value_ : Parser Type.Value
value_ =
    Parser.inContext "value" <|
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
        |= Parser.list { item = value_, separator = spaces }
        |. maybeSpaces
        |. symbol "]"


int : Parser Type.Value
int =
    Parser.int
        |> Parser.map Type.Int


word : Parser Type.Value
word =
    succeed Type.Word
        |. symbol "\""
        |= keep oneOrMore (\c -> c /= ' ' && c /= '\n')


maybeSpaces : Parser ()
maybeSpaces =
    ignore zeroOrMore (\c -> c == ' ')


spaces : Parser ()
spaces =
    ignore oneOrMore (\c -> c == ' ')
