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
import Dict exposing (Dict)
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


type alias FunctionDeclaration =
    { name : String
    , requiredArguments : Int
    , optionalArguments : Int
    }


mapFunctionDeclarations : List Ast.Function -> Dict String FunctionDeclaration
mapFunctionDeclarations functions =
    functions
        |> List.map
            (\{ name, requiredArguments, optionalArguments } ->
                ( name
                , { name = name
                  , requiredArguments =
                        List.length requiredArguments
                  , optionalArguments =
                        List.length optionalArguments
                  }
                )
            )
        |> Dict.fromList


root : Parser Ast.Program
root =
    Parser.inContext "root" <|
        (Parser.repeat zeroOrMore (functionDefinition Dict.empty)
            |> andThen body
        )


body : List Ast.Function -> Parser Ast.Program
body functions =
    let
        knownFunctions =
            mapFunctionDeclarations functions
    in
        statements knownFunctions
            |> Parser.map (Ast.Program functions)


functionDefinition : Dict String FunctionDeclaration -> Parser Ast.Function
functionDefinition knownFunctions =
    Parser.inContext "functionDefinition" <|
        (functionHeader
            |> andThen
                (\( name, requiredArguments, optionalArguments ) ->
                    let
                        knownFunctions_ =
                            Dict.insert name
                                { name = name
                                , requiredArguments =
                                    List.length requiredArguments
                                , optionalArguments =
                                    List.length optionalArguments
                                }
                                knownFunctions
                    in
                        functionBody knownFunctions_
                            |> Parser.map
                                (Ast.Function name
                                    requiredArguments
                                    optionalArguments
                                )
                )
        )


functionHeader : Parser ( String, List String, List ( String, Ast.Node ) )
functionHeader =
    delayedCommit (Parser.keyword "to") <|
        (succeed (\a b c -> ( a, b, c ))
            |. spaces
            |= functionName
            |. spaces
            |= requiredArguments
            |= (oneOf
                    [ delayedCommit spaces <| optionalArguments
                    , succeed []
                    ]
               )
            |. maybeSpaces
            |. symbol "\n"
        )


functionName : Parser String
functionName =
    keep oneOrMore (\c -> c /= ' ' && c /= '\n')


requiredArguments : Parser (List String)
requiredArguments =
    Parser.list { item = requiredArgument, separator = spaces }


requiredArgument : Parser String
requiredArgument =
    succeed identity
        |. symbol ":"
        |= keep oneOrMore (\c -> c /= ' ' && c /= '\n')


optionalArguments : Parser (List ( String, Ast.Node ))
optionalArguments =
    Parser.list { item = optionalArgument, separator = spaces }


optionalArgument : Parser ( String, Ast.Node )
optionalArgument =
    succeed (,)
        |. symbol "["
        |. maybeSpaces
        |= requiredArgument
        |. spaces
        |= expression
        |. maybeSpaces
        |. symbol "]"


functionBody : Dict String FunctionDeclaration -> Parser (List Ast.Node)
functionBody knownFunctions =
    Parser.inContext "functionBody" <|
        (nextLine knownFunctions [])


nextLine : Dict String FunctionDeclaration -> List Ast.Node -> Parser (List Ast.Node)
nextLine knownFunctions acc =
    oneOf
        [ Parser.keyword "end\n"
            |> andThen (\_ -> succeed acc)
        , line knownFunctions
            |> andThen (\line -> nextLine knownFunctions (List.concat [ acc, line ]))
        ]


line : Dict String FunctionDeclaration -> Parser (List Ast.Node)
line knownFunctions =
    Parser.inContext "line" <|
        succeed identity
            |. maybeSpaces
            |= statements knownFunctions
            |. maybeSpaces
            |. symbol "\n"


statements : Dict String FunctionDeclaration -> Parser (List Ast.Node)
statements knownFunctions =
    Parser.list { item = lazy (\_ -> statement knownFunctions), separator = spaces }


statement : Dict String FunctionDeclaration -> Parser Ast.Node
statement knownFunctions =
    Parser.inContext "statement" <|
        oneOf
            [ lazy (\_ -> foreach knownFunctions)
            , lazy (\_ -> repeat knownFunctions)
            , lazy (\_ -> if_ knownFunctions)
            , make
            , variableFunctionCall knownFunctions
            , functionCall knownFunctions
            ]


controlStructure :
    Dict String FunctionDeclaration
    -> String
    ->
        (Ast.Node
         -> List Ast.Node
         -> Ast.Node
        )
    -> Parser Ast.Node
controlStructure knownFunctions keyword constructor =
    Parser.inContext keyword <|
        succeed constructor
            |. Parser.keyword keyword
            |. symbol " "
            |= expression
            |. spaces
            |. symbol "["
            |. maybeSpaces
            |= statements knownFunctions
            |. maybeSpaces
            |. symbol "]"


if_ : Dict String FunctionDeclaration -> Parser Ast.Node
if_ knownFunctions =
    controlStructure knownFunctions "if" Ast.If


foreach : Dict String FunctionDeclaration -> Parser Ast.Node
foreach knownFunctions =
    lazy (\_ -> controlStructure knownFunctions "foreach" Ast.Foreach)


repeat : Dict String FunctionDeclaration -> Parser Ast.Node
repeat knownFunctions =
    controlStructure knownFunctions "repeat" Ast.Repeat


functionCall : Dict String FunctionDeclaration -> Parser Ast.Node
functionCall knownFunctions =
    Parser.inContext "functionCall" <|
        (call
            |> andThen (\name -> lazy (\_ -> functionCall_ knownFunctions name))
        )


functionCall_ : Dict String FunctionDeclaration -> String -> Parser Ast.Node
functionCall_ knownFunctions name =
    let
        command =
            Command.find name

        function =
            Dict.get name knownFunctions
    in
        case ( command, function ) of
            ( Just command, _ ) ->
                commandWithArguments command

            ( _, Just function ) ->
                functionWithArguments function

            _ ->
                Parser.fail "could not parse function call"


{-| In case a function can take a variable number of arguments, you have to use
parentheses around a function call if it does not take the default number of
arguments.

If, e. g., you call `print` with one argument you can do so without parentheses
because the default number of arguments to `print` is 1. If, however, you want
to supply more than one argument, you have to use parentheses, because
otherwise the parser cannot know how many arguments you want to supply.

    print 1 ; can be used without parentheses
    (print 1) ; can be used with parentheses

    print "hello "world ; can’t be used without parentheses
    (print "hello "world) ; has to be used with parentheses

-}
variableFunctionCall : Dict String FunctionDeclaration -> Parser Ast.Node
variableFunctionCall knownFunctions =
    delayedCommit (symbol "(") <|
        ((succeed (,)
            |. maybeSpaces
            |= call
            |= oneOf
                [ succeed identity
                    |. spaces
                    |= Parser.list
                        { item = expression
                        , separator = spaces
                        }
                , succeed []
                ]
            |. maybeSpaces
            |. symbol ")"
         )
            |> andThen (\( a, b ) -> variableFunctionCall_ knownFunctions a b)
        )


variableFunctionCall_ :
    Dict String FunctionDeclaration
    -> String
    -> List Ast.Node
    -> Parser Ast.Node
variableFunctionCall_ knownFunctions name arguments =
    let
        function =
            Dict.get name knownFunctions

        numberOfArguments =
            List.length arguments
    in
        case function of
            Just function ->
                if function.requiredArguments <= numberOfArguments then
                    if
                        numberOfArguments
                            <= function.requiredArguments
                            + function.optionalArguments
                    then
                        succeed <| Ast.Call name arguments
                    else
                        Parser.fail <| "too many inputs to " ++ name
                else
                    Parser.fail <| "not enough inputs to " ++ name

            _ ->
                Parser.fail "could not parse function call"


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


functionWithArguments : FunctionDeclaration -> Parser Ast.Node
functionWithArguments function =
    let
        numberOfArguments =
            function.requiredArguments

        makeNode : List Ast.Node -> Parser Ast.Node
        makeNode result =
            succeed <| Ast.Call function.name result
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
                |= wordOutsideList
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
    Parser.map Ast.Value valueOutsideList


valueOutsideList : Parser Type.Value
valueOutsideList =
    Parser.inContext "valueOutsideList" <|
        oneOf
            [ lazy (\_ -> list)
            , int
            , wordOutsideList
            ]


int : Parser Type.Value
int =
    Parser.int
        |> Parser.map Type.Int


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
        |. maybeSpaces
        |= Parser.list { item = valueInList, separator = spaces }
        |. maybeSpaces
        |. symbol "]"


valueInList : Parser Type.Value
valueInList =
    Parser.inContext "valueInList" <|
        oneOf
            [ lazy (\_ -> list)
            , int
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


maybeSpaces : Parser ()
maybeSpaces =
    ignore zeroOrMore (\c -> c == ' ')


spaces : Parser ()
spaces =
    ignore oneOrMore (\c -> c == ' ')
