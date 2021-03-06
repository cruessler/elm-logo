module Compiler.Ast exposing
    ( CompiledFunction
    , CompiledProgram
    , Context(..)
    , Function
    , Node(..)
    , Program
    , compile
    , compileFunction
    , compileProgram
    )

{-| This module provides types and functions for working with Logo ASTs.
-}

import Dict exposing (Dict)
import List.Nonempty exposing (Nonempty(..))
import Vm.Command as C
import Vm.Error exposing (Error(..))
import Vm.Exception as Exception exposing (Exception)
import Vm.Instruction as Instruction exposing (Instruction(..))
import Vm.Introspect as I
import Vm.Primitive as P
import Vm.Type as Type


type alias Program =
    { functions : List Function
    , body : List Node
    }


{-| Represent a compiled program.
-}
type alias CompiledProgram =
    { instructions : List Instruction
    , compiledFunctions : List CompiledFunction
    }


type alias Function =
    { name : String
    , requiredArguments : List String
    , optionalArguments : List ( String, Node )
    , body : List Node
    }


{-| Represent a compiled function.
-}
type alias CompiledFunction =
    { name : String
    , requiredArguments : List String
    , optionalArguments : List String
    , instances : List CompiledFunctionInstance
    }


type alias CompiledFunctionInstance =
    { mangledName : String
    , body : List Instruction
    }


type Node
    = Sequence (List Node) Node
    | Repeat Node (List Node)
    | Foreach Node (List Node)
    | If Node (List Node)
    | IfElse Node (List Node) (List Node)
    | Command0 C.Command0
    | Command1 C.Command1 Node
    | Command2 C.Command2 Node Node
    | Primitive1 P.Primitive1 Node
    | Primitive2 P.Primitive2 Node Node
    | Introspect0 I.Introspect0
    | Introspect1 I.Introspect1 Node
    | Call String (List Node)
    | Return (Maybe Node)
    | Make String Node
    | Local String
    | Variable String
    | Value Type.Value
    | Raise Exception


{-| Mangle the name of a function based on its number of arguments. This is
used to disambiguate variants of a function that take different numbers of
arguments.

The declaration

    to foo :bar [:baz "qux]
    end

would yield two functions, "foo1" and "foo2", which take 1 and 2 arguments,
respectively.

-}
mangleName : String -> Int -> String
mangleName name arguments =
    name ++ String.fromInt arguments


{-| Represents the context an AST node can be compiled in.

This is relevant for whether or not to raise exceptions about unused or missing
return values.

If the context is `Statement` the node is expected to not return a value, if it
is `Expression` it is expected to return a value.

-}
type Context
    = Statement
    | Expression { caller : String }


{-| Represents the type of a callee.

This is relevant for whether or not it can be statically determined whether to
raise exceptions about unused or missing return values at runtime.

A `Primitive` always returns a value while a `Command` never does. If the
callee is a `UserDefinedFunction` we can only at runtime determine whether it
returns a value because user-defined functions can contain `ifelse` whose
branches not necessarily have the same type.

-}
type Callee
    = Primitive { name : String }
    | Command { name : String }
    | UserDefinedFunction { name : String }
    | DoesNotApply


{-| Determine the type of a callee.

Return `DoesNotApply` if a node does not represent a call or a value.

-}
typeOfCallee : Node -> Callee
typeOfCallee node =
    case node of
        Sequence _ last ->
            typeOfCallee last

        Repeat _ _ ->
            Command { name = "repeat" }

        Foreach _ _ ->
            Command { name = "foreach" }

        Command0 c ->
            Command { name = c.name }

        Command1 c _ ->
            Command { name = c.name }

        Command2 c _ _ ->
            Command { name = c.name }

        Primitive1 p _ ->
            Primitive { name = p.name }

        Primitive2 p _ _ ->
            Primitive { name = p.name }

        Introspect0 i ->
            Primitive { name = i.name }

        Introspect1 i _ ->
            Primitive { name = i.name }

        Call name _ ->
            UserDefinedFunction { name = name }

        Make _ _ ->
            Command { name = "make" }

        Variable name ->
            Primitive { name = name }

        Value _ ->
            Primitive { name = "value" }

        If _ _ ->
            DoesNotApply

        IfElse _ _ _ ->
            DoesNotApply

        Return _ ->
            DoesNotApply

        Raise _ ->
            DoesNotApply

        Local _ ->
            DoesNotApply


{-| Compile a non-empty branch of a control structure, e. g. of an `if`, to a
list of VM instructions.

All nodes except the last one are compiled as statements while the last one is
compiled in the context of the control structure.

-}
compileNonEmptyBranch : Context -> Nonempty Node -> List Instruction
compileNonEmptyBranch context children =
    compileNonEmptyBranch_ context children []
        |> List.concat


compileNonEmptyBranch_ :
    Context
    -> Nonempty Node
    -> List (List Instruction)
    -> List (List Instruction)
compileNonEmptyBranch_ context children acc =
    case children of
        Nonempty first [] ->
            [ compileInContext context first ]

        Nonempty first (second :: rest) ->
            compileInContext Statement first
                :: compileNonEmptyBranch_ context (Nonempty second rest) acc


{-| Compile the branch of a control structure, e. g. of an `if`, to a list of
VM instructions.

If the branch is empty, it can be statically determined whether to raise an
exception at runtime.

-}
compileBranch : String -> Context -> List Node -> List Instruction
compileBranch controlStructure context children =
    case children of
        [] ->
            case context of
                Statement ->
                    []

                Expression { caller } ->
                    [ Instruction.Raise (Exception.NoOutput caller controlStructure) ]

        first :: rest ->
            compileNonEmptyBranch context (Nonempty first rest)


{-| Compile an AST node to a list of VM instructions.

This function inserts instructions that check at runtime whether or not a
caller expects a user-defind function to return a value, and raise an expection
if the expectation is not met.

    to foo :bar
      print "bar
    end
    print foo "baz ; fails at runtime because `foo` did not return a value

    to foo :bar
      output "bar
    end
    foo "baz ; fails at runtime because `foo` did return a value that was not
             ; passed to another function

If it can statically be determined whether the caller expects a value, the
instruction for raising an exception can be inserted without a check.

    print print 5

    3

-}
compileInContext : Context -> Node -> List Instruction
compileInContext context node =
    let
        instructions =
            compile context node
    in
    case context of
        Statement ->
            case typeOfCallee node of
                UserDefinedFunction _ ->
                    instructions
                        ++ [ CheckReturn
                           , JumpIfFalse 2
                           , Instruction.Raise Exception.NoUseOfValue
                           ]

                Primitive _ ->
                    instructions
                        ++ [ Instruction.Raise Exception.NoUseOfValue ]

                _ ->
                    instructions

        Expression { caller } ->
            case typeOfCallee node of
                UserDefinedFunction { name } ->
                    instructions
                        ++ [ CheckReturn
                           , JumpIfTrue 2
                           , Instruction.Raise (Exception.NoOutput caller name)
                           ]

                Primitive _ ->
                    instructions

                Command { name } ->
                    instructions
                        ++ [ Instruction.Raise (Exception.NoOutput caller name) ]

                DoesNotApply ->
                    instructions


{-| Compile an AST node to a list of VM instructions.
-}
compile : Context -> Node -> List Instruction
compile context node =
    case node of
        Sequence rest last ->
            List.concatMap (compileInContext context) rest
                ++ compileInContext context last

        Repeat times children ->
            let
                compiledTimes =
                    compileInContext (Expression { caller = "repeat" }) times

                compiledChildren =
                    List.concatMap (compileInContext Statement) children

                body =
                    [ [ PushLoopScope
                      , EnterLoopScope
                      , JumpIfTrue (List.length compiledChildren + 2)
                      ]
                    , compiledChildren
                    , [ Jump (List.length compiledChildren + 2 |> negate)
                      , PopLoopScope
                      ]
                    ]
                        |> List.concat
            in
            [ compiledTimes
            , [ Duplicate
              , Eval1 { name = "integerp", f = P.integerp }
              , JumpIfTrue 2
              , Instruction.Raise (Exception.WrongInput "repeat")
              ]
            , body
            ]
                |> List.concat

        Foreach iterator children ->
            let
                compiledIterator =
                    compileInContext (Expression { caller = "foreach" }) iterator

                compiledChildren =
                    List.concatMap (compileInContext Statement) children
            in
            [ compiledIterator
            , [ PushTemplateScope
              , EnterTemplateScope
              , JumpIfTrue (List.length compiledChildren + 2)
              ]
            , compiledChildren
            , [ Jump (List.length compiledChildren + 2 |> negate)
              , PopTemplateScope
              ]
            ]
                |> List.concat

        If condition children ->
            let
                compiledCondition =
                    compileInContext (Expression { caller = "if" }) condition

                compiledChildren =
                    compileBranch "if" context children
            in
            [ compiledCondition
            , [ Duplicate
              , Eval1 { name = "boolp", f = P.boolp }
              , JumpIfTrue 2
              , Instruction.Raise (Exception.WrongInput "if")
              , JumpIfFalse (List.length compiledChildren + 1)
              ]
            , compiledChildren
            ]
                |> List.concat

        IfElse condition ifBranch elseBranch ->
            let
                compiledCondition =
                    compileInContext (Expression { caller = "ifelse" }) condition

                compiledIfBranch =
                    compileBranch "ifelse" context ifBranch

                compiledElseBranch =
                    compileBranch "ifelse" context elseBranch
            in
            [ compiledCondition
            , [ Duplicate
              , Eval1 { name = "boolp", f = P.boolp }
              , JumpIfTrue 2
              , Instruction.Raise (Exception.WrongInput "ifelse")
              , JumpIfFalse (List.length compiledIfBranch + 2)
              ]
            , compiledIfBranch
            , [ Jump (List.length compiledElseBranch + 1) ]
            , compiledElseBranch
            ]
                |> List.concat

        Command0 c ->
            [ Instruction.Command0 c ]

        Command1 c node_ ->
            [ compileInContext (Expression { caller = c.name }) node_
            , [ Instruction.Command1 c ]
            ]
                |> List.concat

        Command2 c first second ->
            [ compileInContext (Expression { caller = c.name }) second
            , compileInContext (Expression { caller = c.name }) first
            , [ Instruction.Command2 c ]
            ]
                |> List.concat

        Primitive1 p node_ ->
            [ compileInContext (Expression { caller = p.name }) node_
            , [ Eval1 p ]
            ]
                |> List.concat

        Primitive2 p first second ->
            [ compileInContext (Expression { caller = p.name }) second
            , compileInContext (Expression { caller = p.name }) first
            , [ Eval2 p ]
            ]
                |> List.concat

        Introspect0 i ->
            [ Instruction.Introspect0 i ]

        Introspect1 i node_ ->
            [ compileInContext (Expression { caller = i.name }) node_
            , [ Instruction.Introspect1 i ]
            ]
                |> List.concat

        Call name arguments ->
            let
                mangledName =
                    mangleName name (List.length arguments)
            in
            [ List.reverse arguments
                |> List.concatMap (compileInContext (Expression { caller = name }))
            , [ Instruction.CallByName mangledName ]
            ]
                |> List.concat

        Return (Just node_) ->
            [ compileInContext (Expression { caller = "output" }) node_
            , [ PopLocalScope
              , Instruction.Return
              ]
            ]
                |> List.concat

        Return Nothing ->
            [ PushVoid
            , PopLocalScope
            , Instruction.Return
            ]

        Make name node_ ->
            [ compileInContext (Expression { caller = "make" }) node_
            , [ StoreVariable name ]
            ]
                |> List.concat

        Local name ->
            [ LocalVariable name ]

        Variable name ->
            [ PushVariable name ]

        Value value ->
            [ PushValue value ]

        Raise error ->
            [ Instruction.Raise error ]


compileProgram : Program -> CompiledProgram
compileProgram { functions, body } =
    let
        compiledFunctions =
            List.map compileFunction functions

        instructions =
            List.concatMap (compileInContext Statement) body
    in
    { instructions = instructions
    , compiledFunctions = compiledFunctions
    }


compileRequiredArgument : String -> List Instruction
compileRequiredArgument arg =
    [ LocalVariable arg, StoreVariable arg ]


compileOptionalArgument : Context -> ( String, Node ) -> List Instruction
compileOptionalArgument context ( arg, node ) =
    [ [ LocalVariable arg ]
    , compileInContext context node
    , [ StoreVariable arg ]
    ]
        |> List.concat


{-| Compile a function, taking optional arguments into account by returning
multiple versions, each taking a different number of arguments.
-}
compileFunctionInstances : Function -> List CompiledFunctionInstance
compileFunctionInstances { name, requiredArguments, optionalArguments, body } =
    let
        {- Instructions for required arguments. -}
        instructionsForRequiredArguments =
            [ [ PushLocalScope ]
            , List.concatMap compileRequiredArgument requiredArguments
            ]
                |> List.concat

        {- Instructions for the function body. -}
        instructionsForBody =
            [ List.concatMap (compileInContext Statement) body
            , [ PushVoid
              , PopLocalScope
              , Instruction.Return
              ]
            ]
                |> List.concat

        numberOfRequiredArguments =
            List.length requiredArguments

        numberOfOptionalArguments =
            List.length optionalArguments

        {- Instructions if an optional argument is compiled like a required
           argument. In this case, the value for the argument is expected to be
           passed on the stack.
        -}
        instructionsRequired =
            List.map (Tuple.first >> compileRequiredArgument) optionalArguments

        {- Instructions if an optional argument is compiled as optional
           argument. In this case, the value for the argument is computed by
           the callee itself.
        -}
        instructionsOptional =
            List.map (compileOptionalArgument (Expression { caller = name })) optionalArguments

        {- Given a number of optional arguments to be compiled as required
           arguments, compile a function body.
        -}
        compileBody : Int -> List Instruction
        compileBody i =
            let
                instructions =
                    List.take i instructionsRequired
                        ++ List.drop i instructionsOptional
                        |> List.concat
            in
            [ instructionsForRequiredArguments
            , instructions
            , instructionsForBody
            ]
                |> List.concat
    in
    {- Compile the function for each number of arguments. If, e. g., a
       function takes 1 required and 2 optional arguments, the list returned by
       this function will contain a function for each of the following cases:

         - 1 required argument, 2 optional arguments,
         - 2 required arguments, 1 optional argument,
         - 3 required arguments.
    -}
    List.map
        (\i ->
            { mangledName = mangleName name (numberOfRequiredArguments + i)
            , body = compileBody i
            }
        )
        (List.range 0 numberOfOptionalArguments |> List.reverse)


{-| Compile a function.
-}
compileFunction : Function -> CompiledFunction
compileFunction ({ name, requiredArguments, optionalArguments } as function) =
    { name = name
    , requiredArguments = requiredArguments
    , optionalArguments = List.map Tuple.first optionalArguments
    , instances = compileFunctionInstances function
    }
