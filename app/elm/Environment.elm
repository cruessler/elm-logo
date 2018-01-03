module Environment
    exposing
        ( Environment
        , empty
        , print
        )

{-| This module contains types and functions related to the state of a Logo
environment: the state of the turtle, console output etc.
-}

import Array exposing (Array)
import Vm.Scope exposing (Scope)


type alias Environment =
    { lines : Array String
    }


empty : Environment
empty =
    { lines = Array.empty
    }


{-| Append a line to the console output.
-}
print : String -> Environment -> Environment
print string env =
    { env | lines = Array.push string env.lines }
