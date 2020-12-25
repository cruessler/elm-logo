module Vm.Exception exposing (Exception(..))


type Exception
    = WrongInput String
    | NoUseOfValue
    | NoOutput String String
    | OutputOutsideFunction
    | NotEnoughInputs String
    | TooManyInputs String
    | FunctionAlreadyDefined String
    | CallableUndefined String
