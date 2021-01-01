module Test.Helper exposing (printsLines)

import Environment.History exposing (Entry(..), History)
import Expect
import Logo
import Test exposing (Test, test)


outputLines : List String -> History
outputLines =
    List.indexedMap (\i line -> ( i + 1, Output line ))


printsLines : String -> List String -> Test
printsLines program lines =
    let
        isOutput : ( Int, Entry ) -> Bool
        isOutput entry =
            case entry of
                ( _, Output _ ) ->
                    True

                _ ->
                    False
    in
    test program <|
        \_ ->
            let
                logo =
                    Logo.run program Logo.empty

                history =
                    Logo.getHistory logo

                last =
                    List.head history
            in
            case last of
                Just ( _, Error message ) ->
                    Expect.fail message

                _ ->
                    Expect.equal
                        (lines |> outputLines |> List.reverse)
                        (List.filter isOutput history)
