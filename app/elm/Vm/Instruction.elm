module Vm.Instruction exposing (Instruction(..))

import Vm.Command as C
import Vm.Exception exposing (Exception)
import Vm.Introspect as I
import Vm.Primitive as P
import Vm.Type as Type


{-| Represent instructions a `Vm` can execute.
-}
type Instruction
    = PushValue Type.Value
    | PushVariable String
    | StoreVariable String
    | LocalVariable String
    | Introspect0 I.Introspect0
    | Introspect1 I.Introspect1
    | Eval
    | Eval1 P.Primitive1
    | Eval2 P.Primitive2
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
