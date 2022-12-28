module Compiler.Parser exposing
    ( State
    , arithmeticExpression
    , booleanExpression
    , defaultState
    , functionDefinition
    , output
    , term
    , withExistingFunctions
    )

{-| This module provides functions for parsing Logo code.
-}

import Char
import Compiler.Ast as Ast exposing (CompiledFunction)
import Compiler.Ast.Introspect as Introspect
import Compiler.Parser.Callable as Callable
import Compiler.Parser.Context exposing (Context(..))
import Compiler.Parser.Helper as Helper
import Compiler.Parser.Problem exposing (Problem(..))
import Compiler.Parser.Value as Value
import Dict exposing (Dict)
import Parser.Advanced as P
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        )
import Vm.Exception as Exception
import Vm.Primitive as Primitive
import Vm.Type as Type


type alias State =
    { newFunctions : Dict String Ast.Function
    , existingFunctions : Dict String CompiledFunction
    , parsedBody : List Ast.Node
    , inFunction : Bool
    }


defaultState : State
defaultState =
    { newFunctions = Dict.empty
    , existingFunctions = Dict.empty
    , parsedBody = []
    , inFunction = False
    }


withExistingFunctions : Dict String CompiledFunction -> Parser Context Problem Ast.Program
withExistingFunctions existingFunctions =
    let
        state =
            { defaultState | existingFunctions = existingFunctions }
    in
    P.inContext Root <|
        toplevel state


toplevel : State -> Parser Context Problem Ast.Program
toplevel state =
    let
        program =
            { functions = Dict.values state.newFunctions
            , body = state.parsedBody
            }
    in
    P.inContext Toplevel <|
        P.oneOf
            [ P.end ExpectingEnd
                |> P.map (always program)
            , toplevel_ state
                |> P.andThen toplevel
            ]


toplevel_ : State -> Parser Context Problem State
toplevel_ state =
    P.succeed identity
        |. Helper.maybeSpaces
        |= P.oneOf
            [ function state
            , toplevelStatements state
            , P.succeed state
            ]


defineFunctionUnlessDefined : State -> Ast.Function -> State
defineFunctionUnlessDefined state newFunction =
    if
        Dict.member newFunction.name state.newFunctions
            || Dict.member newFunction.name state.existingFunctions
    then
        let
            parsedBody =
                state.parsedBody
                    ++ [ Ast.Raise (Exception.FunctionAlreadyDefined newFunction.name) ]
        in
        { state | parsedBody = parsedBody }

    else
        let
            newFunctions =
                Dict.insert newFunction.name newFunction state.newFunctions
        in
        { state | newFunctions = newFunctions }


function : State -> Parser Context Problem State
function state =
    functionDefinition state
        |> P.map (defineFunctionUnlessDefined state)


toplevelStatements : State -> Parser Context Problem State
toplevelStatements state =
    let
        appendNodes nodes =
            { state | parsedBody = state.parsedBody ++ nodes }
    in
    P.succeed appendNodes
        |= statements state
        |. Helper.maybeSpaces
        |. P.oneOf [ P.symbol (P.Token "\n" ExpectingNewline), P.end ExpectingEnd ]


defineFunction : State -> Ast.Function -> Parser Context Problem Ast.Function
defineFunction state newFunction =
    let
        newFunctions =
            Dict.insert newFunction.name newFunction state.newFunctions

        -- `temporaryState` never leaves this function. It is only used while
        -- the function body is parsed to enable parsing of recursive
        -- functions.
        temporaryState =
            { state
                | newFunctions = newFunctions
                , inFunction = True
            }
    in
    functionBody temporaryState
        |> P.map (\body -> { newFunction | body = body })


functionDefinition : State -> Parser Context Problem Ast.Function
functionDefinition state =
    P.inContext FunctionDefinition <|
        (functionHeader state
            |> P.andThen (defineFunction state)
        )


functionHeader : State -> Parser Context Problem Ast.Function
functionHeader state =
    P.succeed Ast.Function
        |. Helper.keyword "to"
        |. Helper.spaces
        |= Helper.functionName
        |= P.oneOf
            [ P.succeed identity
                |. P.backtrackable Helper.spaces
                |= requiredArguments
            , P.succeed []
            ]
        |= P.oneOf
            [ P.succeed identity
                |. P.backtrackable Helper.spaces
                |= optionalArguments state
            , P.succeed []
            ]
        |. Helper.maybeSpaces
        |. Helper.symbol "\n"
        |= P.succeed []


requiredArguments : Parser Context Problem (List String)
requiredArguments =
    Helper.list { item = requiredArgument, separator = Helper.spaces }


requiredArgument : Parser Context Problem String
requiredArgument =
    P.succeed identity
        |. Helper.symbol ":"
        |= (P.getChompedString <|
                P.chompWhile (\c -> c /= ' ' && c /= '\n')
           )


optionalArguments : State -> Parser Context Problem (List ( String, Ast.Node ))
optionalArguments state =
    Helper.list { item = optionalArgument state, separator = Helper.spaces }


optionalArgument : State -> Parser Context Problem ( String, Ast.Node )
optionalArgument state =
    P.succeed (\a b -> ( a, b ))
        |. Helper.symbol "["
        |. Helper.maybeSpaces
        |= requiredArgument
        |. Helper.spaces
        |= booleanExpression state
        |. Helper.maybeSpaces
        |. Helper.symbol "]"


functionBody : State -> Parser Context Problem (List Ast.Node)
functionBody state =
    P.inContext FunctionBody <|
        functionBody_ state []


functionBody_ : State -> List Ast.Node -> Parser Context Problem (List Ast.Node)
functionBody_ state acc =
    P.oneOf
        [ P.token (P.Token "end\n" <| ExpectingEnd)
            |> P.map (always acc)
        , line state
            |> P.andThen (\newLine -> functionBody_ state (acc ++ newLine))
        ]


line : State -> Parser Context Problem (List Ast.Node)
line state =
    P.inContext Line <|
        P.succeed identity
            |. Helper.maybeSpaces
            |= statements state
            |. Helper.maybeSpaces
            |. P.token (P.Token "\n" <| ExpectingNewline)


{-| Parse anything that’s valid where statements are expected.

See comment to `statement` for details on why `statements` consists of
expressions.

-}
statements : State -> Parser Context Problem (List Ast.Node)
statements state =
    P.inContext Statements <|
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
statement : State -> Parser Context Problem Ast.Node
statement state =
    P.inContext Statement <|
        P.oneOf
            [ inParentheses state
            , P.lazy (\_ -> ifElse state)
            , P.lazy (\_ -> foreach state)
            , P.lazy (\_ -> map state)
            , P.lazy (\_ -> filter state)
            , P.lazy (\_ -> repeat state)
            , P.lazy (\_ -> until state)
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


output : State -> Parser Context Problem Ast.Node
output state =
    let
        makeNode expr =
            if state.inFunction then
                Ast.Return <| Just expr

            else
                Ast.Raise <| Exception.OutputOutsideFunction
    in
    P.inContext Output <|
        P.succeed makeNode
            |. Helper.keyword "output"
            |. Helper.spaces
            |= booleanExpression state


stop : Parser Context Problem Ast.Node
stop =
    P.succeed (Ast.Return Nothing) |. Helper.keyword "stop"


controlStructure :
    State
    -> { keyword : String, constructor : Ast.Node -> List Ast.Node -> Ast.Node }
    -> Parser Context Problem Ast.Node
controlStructure state { keyword, constructor } =
    P.inContext (Keyword keyword) <|
        P.succeed constructor
            |. Helper.keyword keyword
            |. Helper.spaces
            |= booleanExpression state
            |. Helper.spaces
            |= instructionList state


invertedControlStructure :
    State
    -> { keyword : String, constructor : Ast.Node -> List Ast.Node -> Ast.Node }
    -> Parser Context Problem Ast.Node
invertedControlStructure state { keyword, constructor } =
    P.inContext (Keyword keyword) <|
        P.succeed (\a b -> constructor b a)
            |. Helper.keyword keyword
            |. Helper.spaces
            |= instructionList state
            |. Helper.spaces
            |= booleanExpression state


controlStructure2 :
    State
    ->
        { keyword : String
        , constructor : Ast.Node -> List Ast.Node -> List Ast.Node -> Ast.Node
        }
    -> Parser Context Problem Ast.Node
controlStructure2 state { keyword, constructor } =
    P.inContext (Keyword keyword) <|
        P.succeed constructor
            |. Helper.keyword keyword
            |. Helper.spaces
            |= booleanExpression state
            |. Helper.spaces
            |= instructionList state
            |. Helper.maybeSpaces
            |= instructionList state


instructionList : State -> Parser Context Problem (List Ast.Node)
instructionList state =
    P.succeed identity
        |. Helper.symbol "["
        |. Helper.maybeSpaces
        |= statements state
        |. Helper.maybeSpaces
        |. Helper.symbol "]"


if_ : State -> Parser Context Problem Ast.Node
if_ state =
    controlStructure state { keyword = "if", constructor = Ast.If }


foreach : State -> Parser Context Problem Ast.Node
foreach state =
    P.lazy (\_ -> controlStructure state { keyword = "foreach", constructor = Ast.Foreach })


map : State -> Parser Context Problem Ast.Node
map state =
    P.lazy (\_ -> invertedControlStructure state { keyword = "map", constructor = Ast.Map })


filter : State -> Parser Context Problem Ast.Node
filter state =
    P.lazy (\_ -> invertedControlStructure state { keyword = "filter", constructor = Ast.Filter })


repeat : State -> Parser Context Problem Ast.Node
repeat state =
    controlStructure state { keyword = "repeat", constructor = Ast.Repeat }


until : State -> Parser Context Problem Ast.Node
until state =
    controlStructure state { keyword = "until", constructor = Ast.Until }


ifElse : State -> Parser Context Problem Ast.Node
ifElse state =
    controlStructure2 state { keyword = "ifelse", constructor = Ast.IfElse }


functionCall : State -> Parser Context Problem Ast.Node
functionCall state =
    P.inContext FunctionCall <|
        (Helper.functionName
            |> P.andThen (\name -> P.lazy (\_ -> functionCall_ state name))
        )


functionCall_ : State -> String -> Parser Context Problem Ast.Node
functionCall_ state name =
    let
        functions =
            { newFunctions = state.newFunctions
            , existingFunctions = state.existingFunctions
            }
    in
    Callable.find functions name
        |> Maybe.map
            (\callable ->
                let
                    numberOfArguments =
                        Callable.numberOfDefaultArguments callable
                in
                arguments state numberOfArguments
                    |> P.andThen (\arguments_ -> Callable.makeNode arguments_ callable)
            )
        |> Maybe.withDefault (P.succeed <| Ast.Raise (Exception.CallableUndefined name))


{-| Parse a list of `count` or fewer arguments. We want errors related to
functions being passed too few arguments to be runtime errors instead of parse
errors. For this reason, we parse the argument list even if it has too few
arguments. Then, when we compile the resulting AST nodes to instructions,
we insert an instruction for raising an exception at the right place.
-}
arguments : State -> Int -> Parser Context Problem (List Ast.Node)
arguments state count =
    if count > 0 then
        P.inContext (Arguments count) <|
            P.oneOf
                [ P.succeed identity
                    |. P.backtrackable Helper.spaces
                    |= Helper.repeatAtMost count
                        { item = booleanExpression state
                        , separator = Helper.spaces
                        }
                , P.succeed []
                ]

    else
        P.succeed []


inParentheses : State -> Parser Context Problem Ast.Node
inParentheses state =
    P.inContext InParentheses <|
        P.succeed identity
            |. P.backtrackable (Helper.symbol "(")
            |. Helper.maybeSpaces
            |= P.oneOf
                [ P.lazy (\_ -> functionCallInParentheses state)
                , booleanExpression state
                ]
            |. Helper.maybeSpaces
            |. Helper.symbol ")"


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
functionCallInParentheses : State -> Parser Context Problem Ast.Node
functionCallInParentheses state =
    let
        zeroOrMoreArguments : Parser Context Problem (List Ast.Node)
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
    P.succeed (\a b -> ( a, b ))
        |= P.backtrackable Helper.functionName
        |= zeroOrMoreArguments
        |> P.andThen (\( a, b ) -> variableFunctionCall state a b)


variableFunctionCall : State -> String -> List Ast.Node -> Parser Context Problem Ast.Node
variableFunctionCall state name arguments_ =
    let
        functions =
            { newFunctions = state.newFunctions
            , existingFunctions = state.existingFunctions
            }
    in
    Callable.find functions name
        |> Maybe.map (Callable.makeNode arguments_)
        |> Maybe.withDefault (P.problem <| InvalidFunctionCall name)


type alias BinaryOperator =
    { operator : Parser Context Problem ()
    , constructor : Ast.Node -> Ast.Node -> Ast.Node
    }


type alias BinaryOperation =
    { operators : List BinaryOperator
    , operand : State -> Parser Context Problem Ast.Node
    }


leftAssociative : BinaryOperation -> State -> Parser Context Problem Ast.Node
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
leftAssociative_ : BinaryOperation -> State -> Ast.Node -> Parser Context Problem Ast.Node
leftAssociative_ op state left =
    let
        right : BinaryOperator -> Parser Context Problem Ast.Node
        right { operator, constructor } =
            (P.succeed (constructor left)
                |. P.backtrackable (Helper.maybeSpaces |. operator)
                |. Helper.maybeSpaces
                |= op.operand state
            )
                |> P.andThen (\node -> leftAssociative_ op state node)

        rights : List (Parser Context Problem Ast.Node)
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
booleanExpression : State -> Parser Context Problem Ast.Node
booleanExpression state =
    P.inContext BooleanExpression <|
        leftAssociative
            { operators =
                [ BinaryOperator (Helper.operator "=") <|
                    Ast.Primitive2 { name = "=", f = Primitive.equalp }
                , BinaryOperator (Helper.operator "<>") <|
                    Ast.Primitive2 { name = "<>", f = Primitive.notequalp }
                , BinaryOperator (Helper.operator ">") <|
                    Ast.Primitive2 { name = ">", f = Primitive.greaterp }
                ]
            , operand = arithmeticExpression
            }
            state


arithmeticExpression : State -> Parser Context Problem Ast.Node
arithmeticExpression state =
    P.inContext ArithmeticExpression <|
        leftAssociative
            { operators =
                [ BinaryOperator (Helper.operator "+") <|
                    Ast.Primitive2 { name = "+", f = Primitive.sum2 }
                , BinaryOperator (Helper.operator "-") <|
                    Ast.Primitive2 { name = "-", f = Primitive.difference }
                ]
            , operand = term
            }
            state


term : State -> Parser Context Problem Ast.Node
term state =
    P.inContext Term <|
        leftAssociative
            { operators =
                [ BinaryOperator (Helper.operator "*") <|
                    Ast.Primitive2 { name = "*", f = Primitive.product }
                , BinaryOperator (Helper.operator "/") <|
                    Ast.Primitive2 { name = "/", f = Primitive.quotient }
                ]
            , operand = statement
            }
            state


templateVariable : Parser Context Problem Ast.Node
templateVariable =
    let
        digits =
            P.succeed ()
                |. P.chompIf Char.isDigit ExpectingDigit
                |. P.chompWhile Char.isDigit
                |> P.getChompedString

        makeNode : String -> Ast.Node
        makeNode argument =
            Ast.Introspect1
                Introspect.templateVariable
                (Ast.Value <| Type.Word argument)
    in
    P.inContext TemplateVariable <|
        P.succeed makeNode
            |. Helper.symbol "?"
            |. Helper.maybeSpaces
            |= P.oneOf
                [ P.succeed "rest"
                    |. Helper.keyword "rest"
                , digits
                , P.succeed "1"
                ]


localmake : State -> Parser Context Problem Ast.Node
localmake state =
    let
        makeNode : ( Type.Value, Ast.Node ) -> Parser Context Problem Ast.Node
        makeNode ( name, node ) =
            case name of
                Type.Word word ->
                    P.succeed <| Ast.Sequence [ Ast.Local word ] (Ast.Make word node)

                _ ->
                    P.problem ExpectingWord
    in
    P.inContext Localmake <|
        (P.succeed (\a b -> ( a, b ))
            |. Helper.keyword "localmake"
            |. Helper.spaces
            |= Value.wordOutsideList
            |. Helper.spaces
            |= booleanExpression state
            |> P.andThen makeNode
        )


make : State -> Parser Context Problem Ast.Node
make state =
    let
        makeNode : ( Type.Value, Ast.Node ) -> Parser Context Problem Ast.Node
        makeNode ( name, node ) =
            case name of
                Type.Word word ->
                    P.succeed <| Ast.Make word node

                _ ->
                    P.problem ExpectingWord
    in
    P.inContext Make <|
        (P.succeed (\a b -> ( a, b ))
            |. Helper.keyword "make"
            |. Helper.spaces
            |= Value.wordOutsideList
            |. Helper.spaces
            |= booleanExpression state
            |> P.andThen makeNode
        )


variable : Parser Context Problem Ast.Node
variable =
    P.inContext Variable <|
        P.succeed Ast.Variable
            |. Helper.symbol ":"
            |= (P.getChompedString <|
                    P.chompWhile
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
               )
