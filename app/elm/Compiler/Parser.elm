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
import Compiler.Parser.Helper as Helper
import Dict exposing (Dict)
import Parser as P
    exposing
        ( Parser
        , Count(..)
        , (|.)
        , (|=)
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
    P.inContext "root" <|
        (P.repeat P.zeroOrMore (functionDefinition Dict.empty)
            |> P.andThen body
        )


body : List Ast.Function -> Parser Ast.Program
body functions =
    let
        knownFunctions =
            mapFunctionDeclarations functions
    in
        statements knownFunctions
            |> P.map (Ast.Program functions)


functionDefinition : Dict String FunctionDeclaration -> Parser Ast.Function
functionDefinition knownFunctions =
    P.inContext "functionDefinition" <|
        (functionHeader
            |> P.andThen
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
                            |> P.map
                                (Ast.Function name
                                    requiredArguments
                                    optionalArguments
                                )
                )
        )


functionHeader : Parser ( String, List String, List ( String, Ast.Node ) )
functionHeader =
    P.delayedCommit (P.keyword "to") <|
        (P.succeed (\a b c -> ( a, b, c ))
            |. spaces
            |= functionName
            |. spaces
            |= requiredArguments
            |= (P.oneOf
                    [ P.delayedCommit spaces <| optionalArguments
                    , P.succeed []
                    ]
               )
            |. maybeSpaces
            |. P.symbol "\n"
        )


functionName : Parser String
functionName =
    P.keep P.oneOrMore (\c -> c /= ' ' && c /= '\n')


requiredArguments : Parser (List String)
requiredArguments =
    Helper.list { item = requiredArgument, separator = spaces }


requiredArgument : Parser String
requiredArgument =
    P.succeed identity
        |. P.symbol ":"
        |= P.keep P.oneOrMore (\c -> c /= ' ' && c /= '\n')


optionalArguments : Parser (List ( String, Ast.Node ))
optionalArguments =
    Helper.list { item = optionalArgument, separator = spaces }


optionalArgument : Parser ( String, Ast.Node )
optionalArgument =
    P.succeed (,)
        |. P.symbol "["
        |. maybeSpaces
        |= requiredArgument
        |. spaces
        |= expression
        |. maybeSpaces
        |. P.symbol "]"


functionBody : Dict String FunctionDeclaration -> Parser (List Ast.Node)
functionBody knownFunctions =
    P.inContext "functionBody" <|
        (nextLine knownFunctions [])


nextLine : Dict String FunctionDeclaration -> List Ast.Node -> Parser (List Ast.Node)
nextLine knownFunctions acc =
    P.oneOf
        [ P.keyword "end\n"
            |> P.andThen (\_ -> P.succeed acc)
        , line knownFunctions
            |> P.andThen (\line -> nextLine knownFunctions (List.concat [ acc, line ]))
        ]


line : Dict String FunctionDeclaration -> Parser (List Ast.Node)
line knownFunctions =
    P.inContext "line" <|
        P.succeed identity
            |. maybeSpaces
            |= statements knownFunctions
            |. maybeSpaces
            |. P.symbol "\n"


statements : Dict String FunctionDeclaration -> Parser (List Ast.Node)
statements knownFunctions =
    Helper.list { item = P.lazy (\_ -> statement knownFunctions), separator = spaces }


statement : Dict String FunctionDeclaration -> Parser Ast.Node
statement knownFunctions =
    P.inContext "statement" <|
        P.oneOf
            [ P.lazy (\_ -> foreach knownFunctions)
            , P.lazy (\_ -> repeat knownFunctions)
            , P.lazy (\_ -> if_ knownFunctions)
            , (P.succeed Ast.Return |. P.keyword "stop")
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
    P.inContext keyword <|
        P.succeed constructor
            |. P.keyword keyword
            |. P.symbol " "
            |= expression
            |. spaces
            |. P.symbol "["
            |. maybeSpaces
            |= statements knownFunctions
            |. maybeSpaces
            |. P.symbol "]"


if_ : Dict String FunctionDeclaration -> Parser Ast.Node
if_ knownFunctions =
    controlStructure knownFunctions "if" Ast.If


foreach : Dict String FunctionDeclaration -> Parser Ast.Node
foreach knownFunctions =
    P.lazy (\_ -> controlStructure knownFunctions "foreach" Ast.Foreach)


repeat : Dict String FunctionDeclaration -> Parser Ast.Node
repeat knownFunctions =
    controlStructure knownFunctions "repeat" Ast.Repeat


functionCall : Dict String FunctionDeclaration -> Parser Ast.Node
functionCall knownFunctions =
    P.inContext "functionCall" <|
        (call
            |> P.andThen (\name -> P.lazy (\_ -> functionCall_ knownFunctions name))
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
                P.fail "could not parse function call"


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
    P.delayedCommit (P.symbol "(") <|
        ((P.succeed (,)
            |. maybeSpaces
            |= call
            |= P.oneOf
                [ P.succeed identity
                    |. spaces
                    |= Helper.list
                        { item = expression
                        , separator = spaces
                        }
                , P.succeed []
                ]
            |. maybeSpaces
            |. P.symbol ")"
         )
            |> P.andThen (\( a, b ) -> variableFunctionCall_ knownFunctions a b)
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
                        P.succeed <| Ast.Call name arguments
                    else
                        P.fail <| "too many inputs to " ++ name
                else
                    P.fail <| "not enough inputs to " ++ name

            _ ->
                P.fail "could not parse function call"


commandWithArguments : Command.Command -> Parser Ast.Node
commandWithArguments command =
    let
        numberOfArguments =
            Command.arguments command

        makeNode : List Ast.Node -> Parser Ast.Node
        makeNode result =
            case ( command, result ) of
                ( Command.Command1 command1, [ first ] ) ->
                    P.succeed <| Ast.Command1 command1 first

                _ ->
                    P.fail "could not parse function call"
    in
        expressions numberOfArguments
            |> P.andThen makeNode


functionWithArguments : FunctionDeclaration -> Parser Ast.Node
functionWithArguments function =
    let
        numberOfArguments =
            function.requiredArguments

        makeNode : List Ast.Node -> Parser Ast.Node
        makeNode result =
            P.succeed <| Ast.Call function.name result
    in
        expressions numberOfArguments
            |> P.andThen makeNode


expressions : Int -> Parser (List Ast.Node)
expressions count =
    Helper.repeatExactly count { item = expression, separator = spaces }


expression : Parser Ast.Node
expression =
    P.inContext "expression" <|
        P.delayedCommit maybeSpaces <|
            P.oneOf
                [ templateVariable
                , variable
                , P.lazy (\_ -> primitive)
                , value
                ]


primitive : Parser Ast.Node
primitive =
    P.inContext "primitive" <|
        (call
            |> P.andThen (\name -> P.lazy (\_ -> primitive_ name))
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
                P.fail "could not parse function call"


introspectWithArguments : Introspect.Introspect -> Parser Ast.Node
introspectWithArguments introspect =
    let
        numberOfArguments =
            Introspect.arguments introspect

        makeNode : List Ast.Node -> Parser Ast.Node
        makeNode result =
            case ( introspect, result ) of
                ( Introspect.Introspect1 introspect1, [ first ] ) ->
                    P.succeed <| Ast.Introspect1 introspect1 first

                ( Introspect.Introspect0 introspect0, [] ) ->
                    P.succeed <| Ast.Introspect0 introspect0

                _ ->
                    P.fail "could not parse function call"
    in
        if numberOfArguments == 0 then
            makeNode []
        else
            expressions numberOfArguments
                |> P.andThen makeNode


primitiveWithArguments : Primitive.Primitive -> Parser Ast.Node
primitiveWithArguments primitive =
    let
        numberOfArguments =
            Primitive.arguments primitive

        makeNode : List Ast.Node -> Parser Ast.Node
        makeNode result =
            case ( primitive, result ) of
                ( Primitive.Primitive2 primitive2, [ first, second ] ) ->
                    P.succeed <| Ast.Primitive2 primitive2 first second

                ( Primitive.Primitive1 primitive1, [ first ] ) ->
                    P.succeed <| Ast.Primitive1 primitive1 first

                _ ->
                    P.fail "could not parse function call"
    in
        expressions numberOfArguments
            |> P.andThen makeNode


templateVariable : Parser Ast.Node
templateVariable =
    let
        digits =
            P.keep P.oneOrMore Char.isDigit

        makeNode : String -> Ast.Node
        makeNode argument =
            Ast.Introspect1
                Introspect.templateVariable
                (Ast.Value <| Type.Word argument)
    in
        P.inContext "templateVariable" <|
            P.succeed makeNode
                |. P.symbol "?"
                |. maybeSpaces
                |= P.oneOf [ P.source <| P.keyword "rest", digits, P.succeed "1" ]


call : Parser String
call =
    P.source <|
        P.ignore (Exactly 1)
            (\c ->
                (c /= '[')
                    && (c /= ']')
                    && (c /= '"')
                    && (c /= '\n')
                    && (not <| Char.isDigit c)
            )
            |. P.ignore P.zeroOrMore (\c -> c /= ' ' && c /= '\n')


make : Parser Ast.Node
make =
    let
        makeNode : ( Type.Value, Ast.Node ) -> Parser Ast.Node
        makeNode ( name, node ) =
            case name of
                Type.Word name ->
                    P.succeed <| Ast.Make name node

                _ ->
                    P.fail "make expects the first argument to be a word"
    in
        P.inContext "make" <|
            (P.succeed (,)
                |. P.keyword "make"
                |. spaces
                |= wordOutsideList
                |. spaces
                |= expression
                |> P.andThen makeNode
            )


variable : Parser Ast.Node
variable =
    P.inContext "variable" <|
        P.succeed Ast.Variable
            |. P.symbol ":"
            |= P.keep P.oneOrMore (\c -> c /= ' ' && c /= '\n')


value : Parser Ast.Node
value =
    P.map Ast.Value valueOutsideList


valueOutsideList : Parser Type.Value
valueOutsideList =
    P.inContext "valueOutsideList" <|
        P.oneOf
            [ P.lazy (\_ -> list)
            , int
            , wordOutsideList
            ]


int : Parser Type.Value
int =
    P.int
        |> P.map Type.Int


wordOutsideList : Parser Type.Value
wordOutsideList =
    P.succeed Type.Word
        |. P.symbol "\""
        |= P.keep P.oneOrMore
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
    P.succeed Type.List
        |. P.symbol "["
        |. maybeSpaces
        |= Helper.list { item = valueInList, separator = spaces }
        |. maybeSpaces
        |. P.symbol "]"


valueInList : Parser Type.Value
valueInList =
    P.inContext "valueInList" <|
        P.oneOf
            [ P.lazy (\_ -> list)
            , int
            , wordInList
            ]


wordInList : Parser Type.Value
wordInList =
    P.succeed Type.Word
        |= P.keep P.oneOrMore
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
    P.ignore P.zeroOrMore (\c -> c == ' ')


spaces : Parser ()
spaces =
    P.ignore P.oneOrMore (\c -> c == ' ')
