module Environment.History exposing (Entry(..), History)


type alias History =
    List ( Int, Entry )


type Entry
    = Input String
    | Output String
    | Error String
