module Environment exposing
    ( Environment
    , empty
    , error
    , input
    , print
    )

{-| This module contains types and functions related to the state of a Logo
environment: the state of the turtle, console output etc.
-}

import Array exposing (Array)
import Environment.History exposing (Entry(..))
import Vm.Scope exposing (Scope)


type alias Environment =
    { history : List Entry
    }


empty : Environment
empty =
    { history = []
    }


{-| Append an input to the console output.
-}
input : String -> Environment -> Environment
input string env =
    { env | history = Input string :: env.history }


{-| Append an error to the console output.
-}
error : String -> Environment -> Environment
error string env =
    { env | history = Error string :: env.history }


{-| Append a line to the console output.
-}
print : String -> Environment -> Environment
print string env =
    { env | history = Output string :: env.history }
