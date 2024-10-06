module Tools.Utils exposing (..)

import Http


type alias Size =
    { width : Int, height : Int }


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl str ->
            "BadUrl: " ++ str

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus int ->
            "BadStatus: " ++ String.fromInt int

        Http.BadBody str ->
            "BadBody: " ++ str
