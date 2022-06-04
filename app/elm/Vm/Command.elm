module Vm.Command exposing
    ( Command0
    , Command1
    , Command2
    , CommandN
    , back
    , clean
    , clearscreen
    , forward
    , home
    , left
    , pendown
    , penup
    , print
    , printN
    , right
    , setpencolor
    , setpensize
    , setxy
    , show
    , type_
    )

{-| This module contains types and functions related to Logo’s builtin
commands. A command can take arguments and returns no value.

The functions in this module merely forward to the respective functions in
`Environment` and add a layer of error handling which is why they are not
explicitly documented.

-}

import Environment as E exposing (Environment)
import Vm.Error exposing (Error(..), Internal(..))
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


{-| Represent a builtin command that takes a variable number of arguments.
-}
type alias CommandN =
    { name : String
    , f :
        List Type.Value -> Environment -> Result Error Environment
    , numberOfDefaultArguments : Int
    }


print : Type.Value -> Environment -> Result Error Environment
print value env =
    Ok <| E.print (Type.toString value) env


printN : List Type.Value -> Environment -> Result Error Environment
printN values env =
    let
        value =
            values
                |> List.map Type.toString
                |> String.join " "
    in
    Ok <| E.print value env


show : Type.Value -> Environment -> Result Error Environment
show value env =
    Ok <| E.print (Type.toDebugString value) env


type_ : Type.Value -> Environment -> Result Error Environment
type_ value env =
    Ok <| E.type_ (Type.toString value) env


forward : Type.Value -> Environment -> Result Error Environment
forward value env =
    Type.toFloat value
        |> Result.map (\f -> E.forward f env)
        |> Result.mapError (Internal << Type)


back : Type.Value -> Environment -> Result Error Environment
back value env =
    Type.toFloat value
        |> Result.map (\f -> E.back f env)
        |> Result.mapError (Internal << Type)


left : Type.Value -> Environment -> Result Error Environment
left value env =
    Type.toFloat value
        |> Result.map (\f -> E.left f env)
        |> Result.mapError (Internal << Type)


right : Type.Value -> Environment -> Result Error Environment
right value env =
    Type.toFloat value
        |> Result.map (\f -> E.right f env)
        |> Result.mapError (Internal << Type)


setxy : Type.Value -> Type.Value -> Environment -> Result Error Environment
setxy value1 value2 env =
    Type.toFloat value1
        |> Result.andThen
            (\x ->
                Type.toFloat value2
                    |> Result.map
                        (\y -> E.setxy x y env)
            )
        |> Result.mapError (Internal << Type)


penup : Environment -> Result Error Environment
penup =
    Ok << E.penup


pendown : Environment -> Result Error Environment
pendown =
    Ok << E.pendown


setpencolor : Type.Value -> Environment -> Result Error Environment
setpencolor value env =
    Type.toInt value
        |> Result.map (\int -> E.setpencolor int env)
        |> Result.mapError (Internal << Type)


setpensize : Type.Value -> Environment -> Result Error Environment
setpensize value env =
    Type.toInt value
        |> Result.map (\int -> E.setpensize int env)
        |> Result.mapError (Internal << Type)


home : Environment -> Result Error Environment
home =
    Ok << E.home


clean : Environment -> Result Error Environment
clean =
    Ok << E.clean


clearscreen : Environment -> Result Error Environment
clearscreen =
    Ok << E.clean << E.home
