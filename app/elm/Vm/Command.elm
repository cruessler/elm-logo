module Vm.Command
    exposing
        ( Command0
        , Command1
        , Command2
        , print
        )

{-| This module contains types and functions related to Logo’s builtin
commands. A command can take arguments and returns no value.
-}

import Environment as E exposing (Environment)
import Vm.Error exposing (Error)
import Vm.Type as Type


{-| Represent a builtin command that takes no argument.
-}
type alias Command0 =
    { name : String
    , f :
        Environment -> Result Error Environment
    }


{-| Represent a builtin command that takes one argument.
-}
type alias Command1 =
    { name : String
    , f :
        Type.Value -> Environment -> Result Error Environment
    }


{-| Represent a builtin command that takes two arguments.
-}
type alias Command2 =
    { name : String
    , f :
        Type.Value -> Type.Value -> Environment -> Result Error Environment
    }


{-| Print a value to the console.
-}
print : Type.Value -> Environment -> Result Error Environment
print value env =
    Ok <| E.print (Type.toString value) env
