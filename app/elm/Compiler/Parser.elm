module Compiler.Parser
    exposing
        ( State
        , root
        , output
        , booleanExpression
        , arithmeticExpression
        , term
        , functionDefinition
        )

{-| This module provides functions for parsing Logo code.
-}

import Char
import Compiler.Ast as Ast
import Compiler.Ast.Introspect as Introspect
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
    , parsedBody : List Ast.Node
    , inFunction : Bool
    }


root : Parser Ast.Program
root =
    let
        state =
            { userDefinedFunctions = Dict.empty
            , parsedBody = []
            , inFunction = False
            }
    in
        P.inContext "root" <|
            toplevel state


toplevel : State -> Parser Ast.Program
toplevel state =
    let
        program =
            { functions = Dict.values state.userDefinedFunctions
            , body = state.parsedBody
            }
    in
        P.inContext "toplevel" <|
            (P.oneOf
                [ P.end
                    |> P.map (always program)
                , toplevel_ state
                    |> P.andThen toplevel
                ]
            )


toplevel_ : State -> Parser State
toplevel_ state =
    P.succeed identity
        |. Helper.maybeSpaces
        |= P.oneOf
            [ function state
            , toplevelStatements state
            , P.succeed state
            ]


function : State -> Parser State
function state =
    functionDefinition state
        |> P.map
            (\function ->
                let
                    userDefinedFunctions =
                        Dict.insert function.name function state.userDefinedFunctions
                in
                    { state | userDefinedFunctions = userDefinedFunctions }
            )


toplevelStatements : State -> Parser State
toplevelStatements state =
    let
        appendNodes nodes =
            { state | parsedBody = state.parsedBody ++ nodes }
    in
        P.succeed appendNodes
            |= statements state
            |. Helper.maybeSpaces
            |. P.oneOf [ P.symbol "\n", P.end ]


functionDefinition : State -> Parser Ast.Function
functionDefinition state =
    P.inContext "functionDefinition" <|
        (functionHeader state
            |> P.andThen
                (\({ name, requiredArguments, optionalArguments } as function) ->
                    let
                        userDefinedFunctions =
                            Dict.insert name function state.userDefinedFunctions

                        newState =
                            { state
                                | userDefinedFunctions = userDefinedFunctions
                                , inFunction = True
                            }
                    in
                        functionBody newState
                            |> P.map (\body -> { function | body = body })
                )
        )


functionHeader : State -> Parser Ast.Function
functionHeader state =
    P.delayedCommit (P.keyword "to" |. Helper.spaces) <|
        (P.succeed Ast.Function
            |= Helper.functionName
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
        |= booleanExpression state
        |. Helper.maybeSpaces
        |. P.symbol "]"


functionBody : State -> Parser (List Ast.Node)
functionBody state =
    P.inContext "functionBody" <|
        (functionBody_ state [])


functionBody_ : State -> List Ast.Node -> Parser (List Ast.Node)
functionBody_ state acc =
    P.oneOf
        [ P.keyword "end\n"
            |> P.map (always acc)
        , line state
            |> P.andThen (\line -> functionBody_ state (acc ++ line))
        ]


line : State -> Parser (List Ast.Node)
line state =
    P.inContext "line" <|
        P.succeed identity
            |. Helper.maybeSpaces
            |= statements state
            |. Helper.maybeSpaces
            |. P.symbol "\n"


{-| Parse anything that’s valid where statements are expected.

See comment to `statement` for details on why `statements` consists of
expressions.

-}
statements : State -> Parser (List Ast.Node)
statements state =
    P.inContext "statements" <|
        Helper.list
            { item = P.lazy (\_ -> booleanExpression state)
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
            [ inParentheses state
            , P.lazy (\_ -> ifElse state)
            , P.lazy (\_ -> foreach state)
            , P.lazy (\_ -> repeat state)
            , P.lazy (\_ -> if_ state)
            , P.lazy (\_ -> output state)
            , stop
            , localmake state
            , make state
            , templateVariable
            , variable
            , P.lazy (\_ -> functionCall state)
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
                |= booleanExpression state


stop : Parser Ast.Node
stop =
    P.succeed (Ast.Return Nothing) |. P.keyword "stop"


controlStructure :
    State
    -> { keyword : String, constructor : Ast.Node -> List Ast.Node -> Ast.Node }
    -> Parser Ast.Node
controlStructure state { keyword, constructor } =
    P.inContext keyword <|
        P.succeed constructor
            |. P.keyword keyword
            |. P.symbol " "
            |= booleanExpression state
            |. Helper.spaces
            |= instructionList state


controlStructure2 :
    State
    ->
        { keyword : String
        , constructor : Ast.Node -> List Ast.Node -> List Ast.Node -> Ast.Node
        }
    -> Parser Ast.Node
controlStructure2 state { keyword, constructor } =
    P.inContext keyword <|
        P.succeed constructor
            |. P.keyword keyword
            |. Helper.spaces
            |= booleanExpression state
            |. Helper.spaces
            |= instructionList state
            |. Helper.maybeSpaces
            |= instructionList state


instructionList : State -> Parser (List Ast.Node)
instructionList state =
    P.succeed identity
        |. P.symbol "["
        |. Helper.maybeSpaces
        |= statements state
        |. Helper.maybeSpaces
        |. P.symbol "]"


if_ : State -> Parser Ast.Node
if_ state =
    controlStructure state { keyword = "if", constructor = Ast.If }


foreach : State -> Parser Ast.Node
foreach state =
    P.lazy (\_ -> controlStructure state { keyword = "foreach", constructor = Ast.Foreach })


repeat : State -> Parser Ast.Node
repeat state =
    controlStructure state { keyword = "repeat", constructor = Ast.Repeat }


ifElse : State -> Parser Ast.Node
ifElse state =
    controlStructure2 state { keyword = "ifelse", constructor = Ast.IfElse }


functionCall : State -> Parser Ast.Node
functionCall state =
    P.inContext "function call" <|
        (Helper.functionName
            |> P.andThen (\name -> P.lazy (\_ -> functionCall_ state name))
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
                    { item = booleanExpression state
                    , separator = Helper.spaces
                    }
    else
        P.succeed []


inParentheses : State -> Parser Ast.Node
inParentheses state =
    P.inContext "in parentheses" <|
        P.delayedCommit (P.symbol "(") <|
            P.succeed identity
                |. Helper.maybeSpaces
                |= P.oneOf
                    [ P.lazy (\_ -> functionCallInParentheses state)
                    , booleanExpression state
                    ]
                |. Helper.maybeSpaces
                |. P.symbol ")"


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

The parser only commits if the first thing that follows the opening parenthesis
is a valid function name.

-}
functionCallInParentheses : State -> Parser Ast.Node
functionCallInParentheses state =
    let
        zeroOrMoreArguments : Parser (List Ast.Node)
        zeroOrMoreArguments =
            P.oneOf
                [ P.succeed identity
                    |. Helper.spaces
                    |= Helper.list
                        { item = booleanExpression state
                        , separator = Helper.spaces
                        }
                , P.succeed []
                ]
    in
        P.delayedCommitMap (,) Helper.functionName zeroOrMoreArguments
            |> P.andThen (\( a, b ) -> variableFunctionCall state a b)


variableFunctionCall : State -> String -> List Ast.Node -> Parser Ast.Node
variableFunctionCall state name arguments =
    Callable.find state.userDefinedFunctions name
        |> Maybe.map (Callable.makeNode arguments)
        |> Maybe.withDefault (P.fail <| "could not parse call to function " ++ name)


type alias BinaryOperator =
    { operator : Parser ()
    , constructor : Ast.Node -> Ast.Node -> Ast.Node
    }


type alias BinaryOperation =
    { operators : List BinaryOperator
    , operand : State -> Parser Ast.Node
    }


leftAssociative : BinaryOperation -> State -> Parser Ast.Node
leftAssociative op state =
    P.lazy (\_ -> op.operand state)
        |> P.andThen
            (\left ->
                leftAssociative_ op state left
            )


{-| Parse subsequent child expressions of a binary expression such as addition.

The way this parser is constructed makes sure that `a + b - c` gets parsed as
`(a + b) - c`. The parser first parses the left-most child. If it then
encounters an operator, it constructs a new node and again looks for an
operator and more expressions. See the links for more details.

This is not the most efficient parser because it constructs parsers on the fly.

<https://www.cs.rochester.edu/~nelson/courses/csc_173/grammars/parsetrees.html>
<http://www.cs.cornell.edu/courses/cs211/2006fa/Sections/S3/grammars.html>

-}
leftAssociative_ : BinaryOperation -> State -> Ast.Node -> Parser Ast.Node
leftAssociative_ op state left =
    let
        right : BinaryOperator -> Parser Ast.Node
        right { operator, constructor } =
            (P.delayedCommit (Helper.maybeSpaces |. operator) <|
                P.succeed (constructor left)
                    |. Helper.maybeSpaces
                    |= op.operand state
            )
                |> P.andThen (\node -> leftAssociative_ op state node)

        rights : List (Parser Ast.Node)
        rights =
            List.map right op.operators
    in
        P.oneOf <| rights ++ [ P.succeed left ]


{-| Parse boolean expressions. A boolean expression is the most toplevel
expression.

Operator precedence is encoded in the hierarchy of different parsers. Operators
that bind stronger are parsed by lower parsers.

This means that the AST correctly mirrors the structure of boolean and
arithmetic expressions.

-}
booleanExpression : State -> Parser Ast.Node
booleanExpression state =
    leftAssociative
        { operators =
            [ BinaryOperator (P.keyword "=") Ast.Equal
            , BinaryOperator (P.keyword "<>") Ast.NotEqual
            , BinaryOperator (P.keyword ">") Ast.GreaterThan
            ]
        , operand = arithmeticExpression
        }
        state


arithmeticExpression : State -> Parser Ast.Node
arithmeticExpression state =
    leftAssociative
        { operators =
            [ BinaryOperator (P.keyword "+") Ast.Add
            , BinaryOperator (P.keyword "-") Ast.Subtract
            ]
        , operand = term
        }
        state


term : State -> Parser Ast.Node
term state =
    leftAssociative
        { operators =
            [ BinaryOperator (P.keyword "*") Ast.Multiply
            , BinaryOperator (P.keyword "/") Ast.Divide
            ]
        , operand = statement
        }
        state


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


localmake : State -> Parser Ast.Node
localmake state =
    let
        makeNode : ( Type.Value, Ast.Node ) -> Parser Ast.Node
        makeNode ( name, node ) =
            case name of
                Type.Word name ->
                    P.succeed <| Ast.Sequence [ Ast.Local name ] (Ast.Make name node)

                _ ->
                    P.fail "localmake expects the first argument to be a word"
    in
        P.inContext "localmake" <|
            (P.succeed (,)
                |. P.keyword "localmake"
                |. Helper.spaces
                |= Value.wordOutsideList
                |. Helper.spaces
                |= booleanExpression state
                |> P.andThen makeNode
            )


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
                |= booleanExpression state
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
