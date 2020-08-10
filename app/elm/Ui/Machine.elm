module Ui.Machine exposing (Machine, empty, fromValue)

import Array exposing (Array)
import Json.Decode as D
import Ui.Machine.Environment as Environment exposing (Environment)
import Ui.Machine.Scope as Scope exposing (Binding(..), Scope(..))
import Vm.Stack exposing (Value(..))


type alias Machine =
    { instructions : Array String
    , programCounter : Int
    , stack : List String
    , scopes : List Scope
    , environment : Environment
    }


empty : Machine
empty =
    Machine (Array.fromList []) 0 [] [] Environment.empty


machine : D.Decoder Machine
machine =
    D.map5 Machine
        (D.field "instructions" <| D.array D.string)
        (D.field "programCounter" D.int)
        (D.field "stack" <| D.list D.string)
        (D.field "scopes" <| D.list Scope.scope)
        (D.field "environment" Environment.environment)


fromValue : D.Value -> Result D.Error Machine
fromValue =
    D.decodeValue machine
