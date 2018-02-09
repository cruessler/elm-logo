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
    }


mapFunctionDeclarations : List Ast.Function -> Dict String FunctionDeclaration
mapFunctionDeclarations functions =
    functions
        |> List.map
            (\{ name, requiredArguments } ->
                ( name
                , { name = name
                  , requiredArguments =
                        List.length requiredArguments
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
        |= keep oneOrMore (\c -> c /= ' ' && c /= ']' && c /= '\n')


maybeSpaces : Parser ()
maybeSpaces =
    ignore zeroOrMore (\c -> c == ' ')


spaces : Parser ()
spaces =
    ignore oneOrMore (\c -> c == ' ')
