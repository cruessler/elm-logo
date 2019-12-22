module Vm.Exception exposing (Exception(..))


type Exception
    = WrongInput String
    | NoUseOfValue
    | NoOutput String String
    | OutputOutsideFunction
