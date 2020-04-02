module Vm.Exception exposing (Exception(..), toString)


type Exception
    = WrongInput String
    | NoUseOfValue
    | NoOutput String String
    | OutputOutsideFunction


toString : Exception -> String
toString exception =
    case exception of
        OutputOutsideFunction ->
            "Can only use output inside a procedure"

        NoOutput caller callee ->
            callee ++ " did not output to " ++ caller

        _ ->
            Debug.todo "unimplemented"
