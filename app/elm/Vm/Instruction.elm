module Vm.Instruction exposing (Context(..), Instruction(..))

import Vm.Command as C
import Vm.Exception exposing (Exception)
import Vm.Introspect as I
import Vm.Primitive as P
import Vm.Type as Type


{-| Represents the context a Vm instruction can be executed in.

This is relevant for whether or not to raise exceptions about unused or missing
return values.

If the context is `Statement` the evaluated code is expected to not return a
value, if it is `Expression` it is expected to return a value.

-}
type Context
    = Statement
    | Expression { caller : String }


{-| Represent instructions a `Vm` can execute.
-}
type Instruction
    = PushValue Type.Value
    | PushVariable String
    | StoreVariable String
    | LocalVariable String
    | Make
    | Localmake
    | Local
    | Thing
    | Introspect0 I.Introspect0
    | Introspect1 I.Introspect1
    | EvalInContext Context
    | Eval1 P.Primitive1
    | Eval2 P.Primitive2
    | Eval3 P.Primitive3
    | EvalN P.PrimitiveN Int
    | Command0 C.Command0
    | Command1 C.Command1
    | Command2 C.Command2
    | CommandN C.CommandN Int
    | PushLoopScope
    | EnterLoopScope
    | PopLoopScope
    | PushTemplateScope
    | EnterTemplateScope
    | PopTemplateScope
    | PushLocalScope
    | PopLocalScope
    | JumpIfFalse Int
    | JumpIfTrue Int
    | Jump Int
    | Call Int
    | CallByName String
    | PushVoid
    | Return
    | CheckReturn
    | Duplicate
    | Flip
    | Raise Exception
