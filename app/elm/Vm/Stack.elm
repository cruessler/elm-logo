module Vm.Stack
    exposing
        ( Stack
        , Value(..)
        )

import Vm.Type as Type


type Value
    = Void
    | Address Int
    | Value Type.Value


type alias Stack =
    List Value
