module Editor.Collar exposing (..)

import Collar exposing (Colleer)
import Content exposing (Content)
import Element exposing (Element, text)
import Engine
import Html.Attributes
import Interact
import Json.Encode as E
import PanSvg
import Sound exposing (Sound)
import TypedSvg as S
import TypedSvg.Core as Svg exposing (Svg)
import Wheel exposing (Wheel)


type alias Model =
    { tool : Tool
    , cursor : Int
    , svg : PanSvg.Model
    , interact : Interact.State Interactable
    }


type Tool
    = Play Bool


type Interactable
    = Ignore
    | IReizeHandle Int Bool


fromWheelInteractable : Wheel.Interactable Int -> Interactable
fromWheelInteractable i =
    case i of
        Wheel.IWheel id ->
            Ignore

        Wheel.IResizeHandle id bool ->
            IReizeHandle id bool


init =
    { tool = Play False
    , cursor = 0
    , svg = PanSvg.init
    , interact = Interact.init
    }


type ToUndo
    = Do
    | Group
    | NOOP


type Msg
    = ToggleEngine
    | SoundClicked Sound
    | NewBead (Content Wheel)
    | SvgMsg PanSvg.Msg
    | InteractMsg (Interact.Msg Interactable)


type alias Return =
    { model : Model
    , collar : Colleer
    , toUndo : ToUndo
    , toEngine : Maybe E.Value
    }


update : Msg -> ( Model, Colleer ) -> Return
update msg ( model, collar ) =
    let
        return =
            { model = model
            , collar = collar
            , toUndo = NOOP
            , toEngine = Nothing
            }
    in
    case msg of
        ToggleEngine ->
            { return | toEngine = Just <| Engine.playCollar collar }

        SoundClicked s ->
            { return | collar = Collar.add model.cursor (Collar.beadFromSound s) collar, toUndo = Do }

        NewBead content ->
            return

        SvgMsg subMsg ->
            return

        InteractMsg subMsg ->
            return


viewContent : ( Model, Colleer ) -> Element Msg
viewContent ( model, collar ) =
    Element.html <|
        S.svg
            (List.map (Html.Attributes.map SvgMsg) (PanSvg.svgAttributes model.svg)
                ++ (List.map (Html.Attributes.map InteractMsg) <| Interact.dragSpaceEvents model.interact)
            )
        <|
            List.map (Svg.map <| InteractMsg << Interact.map fromWheelInteractable) <|
                Collar.view collar


viewTools : Model -> Element msg
viewTools model =
    text "TOOLS"
