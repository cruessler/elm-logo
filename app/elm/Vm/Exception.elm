module Vm.Exception exposing (Exception(..))


type Exception
    = WrongInput String
    | NoUseOfValue
    | NoOutput String String
    | OutputOutsideFunction
    | NotEnoughInputs String
    | TooManyInputs String
    | FunctionAlreadyDefined String
    | MacroAlreadyDefined String
    | CallableUndefined String
