module Compiler.Parser.Callable exposing (find, makeNode, numberOfDefaultArguments)

import Compiler.Ast as Ast exposing (CompiledFunction)
import Compiler.Ast.Command as Command
import Compiler.Ast.Introspect as Introspect
import Compiler.Ast.Primitive as Primitive
import Compiler.Parser.Problem exposing (Problem(..))
import Dict exposing (Dict)
import Parser.Advanced exposing (Parser, problem, succeed)
import Vm.Exception as Exception


type Callable
    = Command Command.Command
    | Introspect Introspect.Introspect
    | Primitive Primitive.Primitive
    | NewFunction Ast.Function
    | ExistingFunction CompiledFunction


type alias Function =
    { name : String
    , numberOfRequiredArguments : Int
    , numberOfOptionalArguments : Int
    }


makeNode : List Ast.Node -> Callable -> Parser context Problem Ast.Node
makeNode arguments callable =
    let
        numberOfRequiredArguments =
            numberOfDefaultArguments callable

        numberOfGivenArguments =
            List.length arguments
    in
    if numberOfGivenArguments < numberOfRequiredArguments then
        -- This is incorrect insofar `arguments` are not evaluated before the
        -- exception is raised. `sum print 3`, for example, is supposed to
        -- first print 3 and only then raise an exception. This would require
        -- it to produce AST nodes for `print 3` and not just for raising the
        -- exception.
        --
        -- We would want to use something like the following, but that leads to
        -- problems when compiling arguments that are an expression as in `sum
        -- 3` because if the compiler does not know that `3` is a function
        -- argument, it inserts code that raises an exception if the value goes
        -- unused which results in the error message "You don’t say what to do
        -- with 3" instead of "not enough inputs to sum".
        --
        -- let
        --     raise =
        --         Ast.Raise <| Exception.NotEnoughInputs (name callable)
        -- in
        -- succeed <| Ast.Sequence arguments raise
        succeed <| Ast.Raise <| Exception.NotEnoughInputs (name callable)

    else
        case callable of
            Command command ->
                makeCommand arguments command

            Primitive primitive ->
                makePrimitive arguments primitive

            Introspect introspect ->
                makeIntrospect arguments introspect

            NewFunction function ->
                let
                    numberOfOptionalArguments =
                        List.length function.optionalArguments

                    callableFunction =
                        { name = function.name
                        , numberOfRequiredArguments = numberOfRequiredArguments
                        , numberOfOptionalArguments = numberOfOptionalArguments
                        }
                in
                makeFunction arguments callableFunction

            ExistingFunction function ->
                let
                    numberOfOptionalArguments =
                        List.length function.optionalArguments

                    callableFunction =
                        { name = function.name
                        , numberOfRequiredArguments = numberOfRequiredArguments
                        , numberOfOptionalArguments = numberOfOptionalArguments
                        }
                in
                makeFunction arguments callableFunction


makeCommand : List Ast.Node -> Command.Command -> Parser context Problem Ast.Node
makeCommand arguments command =
    case ( command, arguments ) of
        ( Command.Command0 command0, [] ) ->
            succeed <| Ast.Command0 command0

        ( Command.Command1 command1, [ first ] ) ->
            succeed <| Ast.Command1 command1 first

        ( Command.Command2 command2, [ first, second ] ) ->
            succeed <| Ast.Command2 command2 first second

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


makeFunction : List Ast.Node -> Function -> Parser context Problem Ast.Node
makeFunction arguments function =
    let
        numberOfArguments =
            List.length arguments
    in
    if
        numberOfArguments
            <= function.numberOfRequiredArguments
            + function.numberOfOptionalArguments
    then
        succeed <| Ast.Call function.name arguments

    else
        succeed <| Ast.Raise (Exception.TooManyInputs function.name)


name : Callable -> String
name callable =
    case callable of
        Command command ->
            Command.name command

        Primitive primitive ->
            Primitive.name primitive

        Introspect introspect ->
            Introspect.name introspect

        NewFunction function ->
            function.name

        ExistingFunction function ->
            function.name


type alias Functions =
    { newFunctions : Dict String Ast.Function
    , existingFunctions : Dict String CompiledFunction
    }


find : Functions -> String -> Maybe Callable
find { newFunctions, existingFunctions } name_ =
    let
        command =
            Command.find name_ |> Maybe.map Command

        primitive =
            Primitive.find name_ |> Maybe.map Primitive

        introspect =
            Introspect.find name_ |> Maybe.map Introspect

        function =
            Dict.get name_ newFunctions |> Maybe.map NewFunction

        existingFunction =
            Dict.get name_ existingFunctions |> Maybe.map ExistingFunction
    in
    [ command, primitive, introspect, function, existingFunction ]
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

        NewFunction function ->
            List.length function.requiredArguments

        ExistingFunction function ->
            List.length function.requiredArguments
