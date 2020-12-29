module Environment.History exposing (Entry(..), History, toValue)

import Json.Encode as E


type alias History =
    List ( Int, Entry )


type Entry
    = Input String
    | Output String
    | PartialOutput String
    | Error String


encodeEntry : ( Int, Entry ) -> E.Value
encodeEntry ( id, entry ) =
    case entry of
        Input input_ ->
            E.object
                [ ( "type", E.string "Input" )
                , ( "id", E.int id )
                , ( "input", E.string input_ )
                ]

        Output output_ ->
            E.object
                [ ( "type", E.string "Output" )
                , ( "id", E.int id )
                , ( "output", E.string output_ )
                ]

        PartialOutput output_ ->
            E.object
                [ ( "type", E.string "PartialOutput" )
                , ( "id", E.int id )
                , ( "output", E.string output_ )
                ]

        Error error_ ->
            E.object
                [ ( "type", E.string "Error" )
                , ( "id", E.int id )
                , ( "error", E.string error_ )
                ]


toValue : History -> E.Value
toValue =
    E.list encodeEntry
