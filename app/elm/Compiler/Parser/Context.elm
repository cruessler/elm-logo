module Compiler.Parser.Context exposing (Context(..))


type Context
    = Root
    | Toplevel
    | FunctionDefinition
    | FunctionBody
    | MacroDefinition
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
    | Local
    | Make
    | Thing
    | Variable
    | ValueOutsideList
    | ValueInList
    | BooleanExpression
    | ArithmeticExpression
    | Term
