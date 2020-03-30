module Environment.History exposing (Entry(..))


type Entry
    = Input String
    | Output String
    | Error String
