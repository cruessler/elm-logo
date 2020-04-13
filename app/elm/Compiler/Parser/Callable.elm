module Compiler.Parser.Callable exposing (find, makeNode, numberOfDefaultArguments)

import Compiler.Ast as Ast
import Compiler.Ast.Command as Command
import Compiler.Ast.Introspect as Introspect
import Compiler.Ast.Primitive as Primitive
import Compiler.Parser.Problem exposing (Problem(..))
import Dict exposing (Dict)
import Parser.Advanced exposing (Parser, problem, succeed)
import Vm.Exception as Exception


type alias FunctionDeclaration =
    { name : String
    , requiredArguments : Int
    , optionalArguments : Int
    }


type Callable
    = Command Command.Command
    | Introspect Introspect.Introspect
    | Primitive Primitive.Primitive
    | Function Ast.Function


makeNode : List Ast.Node -> Callable -> Parser context Problem Ast.Node
makeNode arguments callable =
    case callable of
        Command command ->
            makeCommand arguments command

        Primitive primitive ->
            makePrimitive arguments primitive

        Introspect introspect ->
            makeIntrospect arguments introspect

        Function function ->
            makeFunction arguments function


makeCommand : List Ast.Node -> Command.Command -> Parser context Problem Ast.Node
makeCommand arguments command =
    case ( command, arguments ) of
        ( Command.Command1 command1, [ first ] ) ->
            succeed <| Ast.Command1 command1 first

        _ ->
            problem <| InvalidPrimitive (Command.name command)


makePrimitive : List Ast.Node -> Primitive.Primitive -> Parser context Problem Ast.Node
makePrimitive arguments primitive =
    case ( primitive, arguments ) of
        ( Primitive.Primitive1 primitive1, [ first ] ) ->
            succeed <| Ast.Primitive1 primitive1 first

        ( Primitive.Primitive2 primitive2, [ first, second ] ) ->
            succeed <| Ast.Primitive2 primitive2 first second

        _ ->
            problem <| InvalidPrimitive (Primitive.name primitive)


makeIntrospect : List Ast.Node -> Introspect.Introspect -> Parser context Problem Ast.Node
makeIntrospect arguments introspect =
    case ( introspect, arguments ) of
        ( Introspect.Introspect0 introspect0, [] ) ->
            succeed <| Ast.Introspect0 introspect0

        ( Introspect.Introspect1 introspect1, [ first ] ) ->
            succeed <| Ast.Introspect1 introspect1 first

        _ ->
            problem <| InvalidPrimitive (Introspect.name introspect)


makeFunction : List Ast.Node -> Ast.Function -> Parser context Problem Ast.Node
makeFunction arguments function =
    let
        numberOfArguments =
            List.length arguments

        requiredArguments =
            List.length function.requiredArguments

        optionalArguments =
            List.length function.optionalArguments
    in
    if requiredArguments <= numberOfArguments then
        if
            numberOfArguments
                <= requiredArguments
                + optionalArguments
        then
            succeed <| Ast.Call function.name arguments

        else
            succeed <| Ast.Raise (Exception.TooManyInputs function.name)

    else
        succeed <| Ast.Raise (Exception.NotEnoughInputs function.name)


find : Dict String Ast.Function -> String -> Maybe Callable
find userDefinedFunctions name =
    let
        command =
            Command.find name |> Maybe.map Command

        primitive =
            Primitive.find name |> Maybe.map Primitive

        introspect =
            Introspect.find name |> Maybe.map Introspect

        function =
            Dict.get name userDefinedFunctions |> Maybe.map Function
    in
    [ command, primitive, introspect, function ]
        |> List.filterMap identity
        |> List.head


numberOfDefaultArguments : Callable -> Int
numberOfDefaultArguments callable =
    case callable of
        Command command ->
            Command.arguments command

        Primitive primitive ->
            Primitive.arguments primitive

        Introspect introspect ->
            Introspect.arguments introspect

        Function function ->
            List.length function.requiredArguments
