module Editor.Collar exposing (..)

import Collar exposing (Colleer)
import Element exposing (Element, text)
import Interact
import TypedSvg.Core exposing (Svg)
import Wheel


type alias Model =
    { tool : Tool
    , cursor : Int
    }


type Tool
    = Play Bool


init =
    { tool = Play False
    , cursor = 0
    }


viewContent : ( Model, Colleer ) -> List (Svg (Interact.Msg (Wheel.Interactable item)))
viewContent ( model, collar ) =
    Collar.view collar


viewTools : Model -> Element msg
viewTools model =
    text "TOOLS"
