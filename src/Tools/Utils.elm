module Tools.Utils exposing (..)

import Element
import Html.Attributes
import Http


type alias Size =
    { width : Int, height : Int }


htmlId : String -> Element.Attribute msg
htmlId =
    Element.htmlAttribute << Html.Attributes.id


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
