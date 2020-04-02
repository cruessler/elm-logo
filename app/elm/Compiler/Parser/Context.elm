module Compiler.Parser.Context exposing (Context(..))


type Context
    = Root
    | Toplevel
    | FunctionDefinition
    | FunctionBody
    | Line
    | Statements
    | Statement
    | Output
    | Keyword String
    | FunctionCall
    | Arguments Int
    | InParentheses
    | TemplateVariable
    | Localmake
    | Make
    | Variable
    | ValueOutsideList
    | ValueInList
    | BooleanExpression
    | ArithmeticExpression
    | Term
