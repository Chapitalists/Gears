module Editor.Collar exposing (..)

import Collar exposing (Colleer)
import Element exposing (Element, text)
import Engine
import Interact
import Json.Encode as E
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


type ToUndo
    = Do
    | Group
    | NOOP


type Msg
    = ToggleEngine


update : Msg -> ( Model, Colleer ) -> ( Model, ( Colleer, ToUndo ), Maybe E.Value )
update msg ( model, collar ) =
    case msg of
        ToggleEngine ->
            ( model
            , ( collar, NOOP )
            , Just <| Engine.playCollar collar
            )


viewContent : ( Model, Colleer ) -> List (Svg (Interact.Msg (Wheel.Interactable item)))
viewContent ( model, collar ) =
    Collar.view collar


viewTools : Model -> Element msg
viewTools model =
    text "TOOLS"
