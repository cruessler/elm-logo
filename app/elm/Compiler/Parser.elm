module Compiler.Parser
    exposing
        ( root
        , functionDefinition
        )

{-| This module provides functions for parsing Logo code.
-}

import Char
import Compiler.Ast as Ast
import Compiler.Ast.Command as Command
import Compiler.Ast.Introspect as Introspect
import Compiler.Ast.Primitive as Primitive
import Compiler.Parser.Callable as Callable
import Compiler.Parser.Helper as Helper
import Compiler.Parser.Value as Value
import Dict exposing (Dict)
import Parser as P
    exposing
        ( Parser
        , Count(..)
        , (|.)
        , (|=)
        )
import Vm.Exception as Exception
import Vm.Type as Type


type alias State =
    { userDefinedFunctions : Dict String Ast.Function
    , inFunction : Bool
    }


type alias FunctionDeclaration =
    { name : String
    , requiredArguments : Int
    , optionalArguments : Int
    }


mapFunctionDeclarations : List Ast.Function -> Dict String Ast.Function
mapFunctionDeclarations functions =
    functions
        |> List.map (\function -> ( function.name, function ))
        |> Dict.fromList


root : Parser Ast.Program
root =
    let
        state =
            { inFunction = False, userDefinedFunctions = Dict.empty }
    in
        P.inContext "root" <|
            (P.repeat P.zeroOrMore (functionDefinition state)
                |> P.andThen body
            )


body : List Ast.Function -> Parser Ast.Program
body functions =
    let
        userDefinedFunctions =
            mapFunctionDeclarations functions
    in
        statements
            { inFunction = False
            , userDefinedFunctions = userDefinedFunctions
            }
            |> P.map (Ast.Program functions)


functionDefinition : State -> Parser Ast.Function
functionDefinition state =
    P.inContext "functionDefinition" <|
        (functionHeader state
            |> P.andThen
                (\({ name, requiredArguments, optionalArguments } as function) ->
                    let
                        userDefinedFunctions_ =
                            Dict.insert name function state.userDefinedFunctions
                    in
                        functionBody
                            { state
                                | inFunction = True
                                , userDefinedFunctions = userDefinedFunctions_
                            }
                            |> P.map
                                (Ast.Function name
                                    requiredArguments
                                    optionalArguments
                                )
                )
        )


functionHeader : State -> Parser Ast.Function
functionHeader state =
    P.delayedCommit (P.keyword "to" |. Helper.spaces) <|
        (P.succeed Ast.Function
            |= functionName
            |= (P.oneOf
                    [ P.delayedCommit Helper.spaces <| requiredArguments
                    , P.succeed []
                    ]
               )
            |= (P.oneOf
                    [ P.delayedCommit Helper.spaces <| optionalArguments state
                    , P.succeed []
                    ]
               )
            |. Helper.maybeSpaces
            |. P.symbol "\n"
            |= P.succeed []
        )


functionName : Parser String
functionName =
    P.keep P.oneOrMore (\c -> c /= ' ' && c /= '\n')


requiredArguments : Parser (List String)
requiredArguments =
    Helper.list { item = requiredArgument, separator = Helper.spaces }


requiredArgument : Parser String
requiredArgument =
    P.succeed identity
        |. P.symbol ":"
        |= P.keep P.oneOrMore (\c -> c /= ' ' && c /= '\n')


optionalArguments : State -> Parser (List ( String, Ast.Node ))
optionalArguments state =
    Helper.list { item = optionalArgument state, separator = Helper.spaces }


optionalArgument : State -> Parser ( String, Ast.Node )
optionalArgument state =
    P.succeed (,)
        |. P.symbol "["
        |. Helper.maybeSpaces
        |= requiredArgument
        |. Helper.spaces
        |= statement state
        |. Helper.maybeSpaces
        |. P.symbol "]"


functionBody : State -> Parser (List Ast.Node)
functionBody state =
    P.inContext "functionBody" <|
        (nextLine state [])


nextLine : State -> List Ast.Node -> Parser (List Ast.Node)
nextLine state acc =
    P.oneOf
        [ P.keyword "end\n"
            |> P.andThen (\_ -> P.succeed acc)
        , line state
            |> P.andThen (\line -> nextLine state (List.concat [ acc, line ]))
        ]


line : State -> Parser (List Ast.Node)
line state =
    P.inContext "line" <|
        P.succeed identity
            |. Helper.maybeSpaces
            |= statements state
            |. Helper.maybeSpaces
            |. P.symbol "\n"


statements : State -> Parser (List Ast.Node)
statements state =
    Helper.list
        { item = P.lazy (\_ -> statement state)
        , separator = Helper.spaces
        }


{-| Parse anything that’s valid where a statement is expected.

Most of the alternatives are statements, but some are expressions. The
function is named `statement` to avoid having to use `statementOrExpression`
throughout the parser although the latter would be the correct name.

Although an expression is syntactically valid at the top level of a program it
will lead to a runtime error.

This function parses both statements and expressions and leaves it to the later
stages of the compiler to deal with semantically invalid programs.

-}
statement : State -> Parser Ast.Node
statement state =
    P.inContext "statement" <|
        P.oneOf
            [ variableFunctionCall state
            , P.lazy (\_ -> foreach state)
            , P.lazy (\_ -> repeat state)
            , P.lazy (\_ -> if_ state)
            , P.lazy (\_ -> output state)
            , stop
            , make state
            , functionCall state
            , templateVariable
            , variable
            , P.lazy (\_ -> primitive state)
            , Value.value
            ]


output : State -> Parser Ast.Node
output state =
    let
        makeNode expr =
            if state.inFunction then
                Ast.Return <| Just expr
            else
                Ast.Raise <| Exception.OutputOutsideFunction
    in
        P.inContext "output" <|
            P.succeed makeNode
                |. P.keyword "output"
                |. Helper.spaces
                |= statement state


stop : Parser Ast.Node
stop =
    P.succeed (Ast.Return Nothing) |. P.keyword "stop"


controlStructure :
    State
    -> String
    ->
        (Ast.Node
         -> List Ast.Node
         -> Ast.Node
        )
    -> Parser Ast.Node
controlStructure state keyword constructor =
    P.inContext keyword <|
        P.succeed constructor
            |. P.keyword keyword
            |. P.symbol " "
            |= statement state
            |. Helper.spaces
            |. P.symbol "["
            |. Helper.maybeSpaces
            |= statements state
            |. Helper.maybeSpaces
            |. P.symbol "]"


if_ : State -> Parser Ast.Node
if_ state =
    controlStructure state "if" Ast.If


foreach : State -> Parser Ast.Node
foreach state =
    P.lazy (\_ -> controlStructure state "foreach" Ast.Foreach)


repeat : State -> Parser Ast.Node
repeat state =
    controlStructure state "repeat" Ast.Repeat


functionCall : State -> Parser Ast.Node
functionCall state =
    P.inContext "functionCall" <|
        (call
            |> P.andThen
                (\name ->
                    P.succeed identity
                        |. Helper.maybeSpaces
                        |= functionCall_ state name
                )
        )


functionCall_ : State -> String -> Parser Ast.Node
functionCall_ state name =
    Callable.find state.userDefinedFunctions name
        |> Maybe.map
            (\callable ->
                let
                    numberOfArguments =
                        Callable.numberOfArguments callable
                in
                    arguments state numberOfArguments
                        |> P.andThen (\arguments -> Callable.makeNode arguments callable)
            )
        |> Maybe.withDefault (P.fail <| "could not parse call to " ++ name)


arguments : State -> Int -> Parser (List Ast.Node)
arguments state count =
    if count > 0 then
        P.inContext "arguments" <|
            P.delayedCommit Helper.maybeSpaces <|
                Helper.repeatExactly count
                    { item = statement state
                    , separator = Helper.spaces
                    }
    else
        P.succeed []


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
variableFunctionCall : State -> Parser Ast.Node
variableFunctionCall state =
    P.delayedCommit (P.symbol "(") <|
        ((P.succeed (,)
            |. Helper.maybeSpaces
            |= call
            |= P.oneOf
                [ P.succeed identity
                    |. Helper.spaces
                    |= Helper.list
                        { item = P.lazy (\_ -> statement state)
                        , separator = Helper.spaces
                        }
                , P.succeed []
                ]
            |. Helper.maybeSpaces
            |. P.symbol ")"
         )
            |> P.andThen (\( a, b ) -> variableFunctionCall_ state a b)
        )


variableFunctionCall_ : State -> String -> List Ast.Node -> Parser Ast.Node
variableFunctionCall_ state name arguments =
    Callable.find state.userDefinedFunctions name
        |> Maybe.map (Callable.makeNode arguments)
        |> Maybe.withDefault (P.fail <| "could not parse call to function " ++ name)


commandWithArguments : State -> Command.Command -> Parser Ast.Node
commandWithArguments state command =
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
        arguments state numberOfArguments
            |> P.andThen makeNode


functionWithArguments : State -> FunctionDeclaration -> Parser Ast.Node
functionWithArguments state function =
    let
        numberOfArguments =
            function.requiredArguments

        makeNode : List Ast.Node -> Parser Ast.Node
        makeNode result =
            P.succeed <| Ast.Call function.name result
    in
        arguments state numberOfArguments
            |> P.andThen makeNode


primitive : State -> Parser Ast.Node
primitive state =
    P.inContext "primitive" <|
        (call
            |> P.andThen (\name -> P.lazy (\_ -> primitive_ state name))
        )


primitive_ : State -> String -> Parser Ast.Node
primitive_ state name =
    let
        primitive =
            Primitive.find name

        introspect =
            Introspect.find name
    in
        case ( primitive, introspect ) of
            ( Just primitive, _ ) ->
                primitiveWithArguments state primitive

            ( _, Just introspect ) ->
                introspectWithArguments state introspect

            _ ->
                P.fail "could not parse function call"


introspectWithArguments : State -> Introspect.Introspect -> Parser Ast.Node
introspectWithArguments state introspect =
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
            arguments state numberOfArguments
                |> P.andThen makeNode


primitiveWithArguments : State -> Primitive.Primitive -> Parser Ast.Node
primitiveWithArguments state primitive =
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
        arguments state numberOfArguments
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
                |. Helper.maybeSpaces
                |= P.oneOf [ P.source <| P.keyword "rest", digits, P.succeed "1" ]


call : Parser String
call =
    P.source <|
        P.ignore (Exactly 1)
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
            |. P.ignore P.zeroOrMore (\c -> c /= ' ' && c /= '\n')


make : State -> Parser Ast.Node
make state =
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
                |. Helper.spaces
                |= Value.wordOutsideList
                |. Helper.spaces
                |= P.lazy (\_ -> statement state)
                |> P.andThen makeNode
            )


variable : Parser Ast.Node
variable =
    P.inContext "variable" <|
        P.succeed Ast.Variable
            |. P.symbol ":"
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
