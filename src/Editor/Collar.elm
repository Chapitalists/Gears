module Editor.Collar exposing (..)

import Collar exposing (Colleer)
import Color
import Content exposing (Content)
import Editor.Common exposing (..)
import Element exposing (..)
import Element.Input as Input
import Engine
import Html.Attributes
import Interact
import Json.Encode as E
import Math.Vector2 as Vec exposing (vec2)
import PanSvg
import Random
import Sound exposing (Sound)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as Svg exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Length(..))
import Wheel exposing (Wheel)


type alias Model =
    { tool : Tool
    , edit : Maybe Int
    , cursor : Int
    , mode : Mode
    , interact : Interact.State Interactable
    , svg : PanSvg.Model
    }


type Tool
    = Play Bool
    | Edit


type Mode
    = CommonMode CommonMode


keyCodeToMode : List ( String, Mode )
keyCodeToMode =
    []


type Interactable
    = Ignore
    | IBead Int
    | IResizeHandle Int Bool


fromWheelInteractable : Wheel.Interactable Int -> Interactable
fromWheelInteractable i =
    case i of
        Wheel.IWheel id ->
            IBead id

        Wheel.IResizeHandle id bool ->
            IResizeHandle id bool


init : Colleer -> PanSvg.Model -> Model
init c svg =
    { tool = Play False
    , edit = Nothing
    , cursor = 0
    , mode = CommonMode Normal
    , interact = Interact.init
    , svg =
        { svg
            | viewPos =
                { c = Vec.add leftmostPoint <| vec2 (Collar.getTotalLength c / 2) 0
                , smallestSize = 8 * Collar.getMaxLength c
                }
        }
    }


type Msg
    = ChangedTool Tool
    | ChangedMode Mode
    | CursorRight
    | CursorLeft
    | ToggleEngine
    | SoundClicked Sound
    | NewBead (Content Wheel)
    | DeleteBead Int
    | WheelMsg ( Int, Wheel.Msg )
    | SvgMsg PanSvg.Msg
    | OutMsg DocMsg
    | InteractMsg (Interact.Msg Interactable)


type DocMsg
    = Inside Int


type alias Return =
    { model : Model
    , collar : Colleer
    , toUndo : ToUndo
    , toEngine : Maybe E.Value
    , outMsg : Maybe DocMsg
    , cmd : Cmd Msg
    }


update : Msg -> ( Model, Colleer ) -> Return
update msg ( model, collar ) =
    let
        return =
            { model = model
            , collar = collar
            , toUndo = NOOP
            , toEngine = Nothing
            , outMsg = Nothing
            , cmd = Cmd.none
            }
    in
    case msg of
        ChangedTool tool ->
            { return | model = { model | tool = tool } }

        ChangedMode mode ->
            { return | model = { model | mode = mode } }

        CursorRight ->
            { return | model = { model | cursor = min (model.cursor + 1) <| Collar.length collar } }

        CursorLeft ->
            { return | model = { model | cursor = max (model.cursor - 1) 0 } }

        ToggleEngine ->
            { return | toEngine = Just <| Engine.playCollar collar }

        SoundClicked s ->
            update (NewBead <| Content.S s) ( model, collar )

        NewBead c ->
            let
                colorGen =
                    Random.map (\f -> Color.hsl f 1 0.5) <| Random.float 0 1
            in
            { return
                | collar = Collar.add model.cursor (Collar.beadFromContent c) collar
                , toUndo = Do
                , model = { model | cursor = model.cursor + 1 }
                , cmd = Random.generate (\color -> WheelMsg ( model.cursor, Wheel.ChangeColor color )) colorGen
            }

        DeleteBead i ->
            { return
                | collar = Collar.rm i collar
                , toUndo = Do
                , model =
                    { model
                        | cursor =
                            if model.cursor > i then
                                model.cursor - 1

                            else
                                model.cursor
                    }
            }

        WheelMsg ( i, subMsg ) ->
            { return | collar = Collar.updateBead i (Wheel.update subMsg) collar, toUndo = Do }

        SvgMsg subMsg ->
            { return | model = { model | svg = PanSvg.update subMsg model.svg } }

        OutMsg subMsg ->
            { return | outMsg = Just subMsg }

        InteractMsg subMsg ->
            let
                ( interact, event ) =
                    Interact.update subMsg model.interact

                newModel =
                    { model | interact = interact }
            in
            case event of
                Nothing ->
                    { return | model = newModel }

                Just e ->
                    manageInteractEvent e newModel collar


subs : Model -> List (Sub Msg)
subs { interact } =
    (Sub.map SvgMsg <| PanSvg.sub)
        :: (List.map (Sub.map InteractMsg) <| Interact.subs interact)


leftmostPoint : Vec.Vec2
leftmostPoint =
    vec2 0 0


viewContent : ( Model, Colleer ) -> Element Msg
viewContent ( model, collar ) =
    let
        getMod : Int -> Wheel.Mod
        getMod i =
            if model.tool == Edit && model.edit == Just i then
                Wheel.Selected

            else
                case Interact.getInteract model.interact of
                    Just ( IBead j, Interact.Hover ) ->
                        if i == j then
                            Wheel.Selectable

                        else
                            Wheel.None

                    _ ->
                        Wheel.None
    in
    Element.html <|
        S.svg
            (List.map (Html.Attributes.map SvgMsg) (PanSvg.svgAttributes model.svg)
                ++ (List.map (Html.Attributes.map InteractMsg) <| Interact.dragSpaceEvents model.interact)
            )
        <|
            List.map (Svg.map <| InteractMsg << Interact.map fromWheelInteractable)
                (List.foldl
                    (\b ( l, ( p, i ) ) ->
                        ( Wheel.view b.wheel
                            (vec2 (p + b.length / 2) <| Vec.getY leftmostPoint)
                            b.length
                            { mod = getMod i, motor = False, dashed = False }
                            i
                            (Collar.toUID i)
                            :: l
                        , ( p + b.length
                          , i + 1
                          )
                        )
                    )
                    ( [], ( Vec.getX leftmostPoint, 0 ) )
                    (Collar.getBeads collar)
                    |> Tuple.first
                )
                ++ viewCursor model collar


viewCursor : Model -> Colleer -> List (Svg msg)
viewCursor { cursor } c =
    let
        medLength =
            Collar.getMinLength c + Collar.getMaxLength c / 2

        cursorW =
            medLength / 15

        cursorH =
            medLength * 2
    in
    [ S.rect
        [ SA.x <| Num <| Collar.getLengthAt cursor c - cursorW / 2
        , SA.y <| Num <| -cursorH / 2
        , SA.width <| Num cursorW
        , SA.height <| Num cursorH
        , SA.fill <| Fill Color.lightBlue
        ]
        []
    ]


viewTools : Model -> Element Msg
viewTools model =
    Input.radioRow [ spacing 30 ]
        { onChange = ChangedTool
        , options =
            [ Input.option (Play False) <| text "Jeu (W)"
            , Input.option Edit <| text "Édition (C)"
            ]
        , selected = Just model.tool
        , label = Input.labelHidden "Outils"
        }


viewDetails : Model -> Colleer -> List (Element Msg)
viewDetails model c =
    case ( model.tool, model.edit ) of
        ( Edit, Just i ) ->
            let
                b =
                    Collar.get i c
            in
            [ viewDetailsColumn
                [ viewNameInput b (Collar.toUID i) <| \str -> WheelMsg ( i, Wheel.Named str )
                , viewContentButton b <| OutMsg <| Inside i
                , viewVolumeSlider b <| \f -> WheelMsg ( i, Wheel.ChangeVolume f )
                , viewDeleteButton <| DeleteBead i
                ]
            ]

        _ ->
            []


manageInteractEvent : Interact.Event Interactable -> Model -> Colleer -> Return
manageInteractEvent event model collar =
    let
        return =
            { model = model
            , collar = collar
            , toUndo = NOOP
            , toEngine = Nothing
            , outMsg = Nothing
            , cmd = Cmd.none
            }
    in
    case model.mode of
        CommonMode Nav ->
            case ( event.item, event.action ) of
                ( IBead i, Interact.Clicked _ ) ->
                    case Wheel.getContent <| Collar.get i collar of
                        Content.S _ ->
                            return

                        _ ->
                            { return | outMsg = Just <| Inside i }

                _ ->
                    return

        CommonMode Normal ->
            case model.tool of
                Play on ->
                    let
                        scale =
                            PanSvg.getScale model.svg
                    in
                    case ( event.item, event.action ) of
                        -- MUTE
                        ( IBead i, Interact.Clicked _ ) ->
                            let
                                w =
                                    (Collar.get i collar).wheel

                                newMute =
                                    not w.mute
                            in
                            return

                        -- TODO
                        _ ->
                            return

                Edit ->
                    case ( event.item, event.action ) of
                        -- DETAIL
                        ( IBead i, Interact.Clicked _ ) ->
                            { return | model = { model | edit = Just i } }

                        _ ->
                            return
