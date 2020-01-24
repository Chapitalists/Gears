module Editor.Collar exposing (..)

import Coll
import Color
import Data.Collar as Collar exposing (Colleer)
import Data.Common as CommonData
import Data.Content as Content exposing (Content)
import Data.Wheel as Wheel exposing (Wheel, defaultStyle)
import Editor.Common exposing (..)
import Element exposing (..)
import Element.Input as Input
import Engine
import Html.Attributes
import Interact
import Json.Decode as D
import Json.Encode as E
import Math.Vector2 as Vec exposing (vec2)
import PanSvg
import Random
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as Svg exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Length(..))


type alias Model =
    { tool : Tool
    , cursor : Int
    , mode : Mode
    , interact : Interact.State Interactable
    , common : CommonModel
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


init : Colleer -> ( CommonModel, PanSvg.Model ) -> Model
init c ( common, svg ) =
    { tool = Play False
    , cursor = 0
    , mode = CommonMode Normal
    , interact = Interact.init
    , common = commonInit <| Just common
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
    | NewBead (Content Wheel)
    | DeleteBead Int
    | UnpackBead ( Wheel, Float ) Bool
    | ResizeToContent Int
    | WheelMsg ( Int, Wheel.Msg )
    | CommonMsg CommonMsg
    | SvgMsg PanSvg.Msg
    | SVGSize (Result D.Error PanSvg.Size)
    | OutMsg DocMsg
    | InteractMsg (Interact.Msg Interactable Zone)


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
            { return | model = { model | tool = tool }, toEngine = Just Engine.stop }

        ChangedMode mode ->
            { return | model = { model | mode = mode } }

        CursorRight ->
            { return | model = { model | cursor = min (model.cursor + 1) <| Collar.length collar } }

        CursorLeft ->
            { return | model = { model | cursor = max (model.cursor - 1) 0 } }

        ToggleEngine ->
            { return | toEngine = Just <| Engine.playCollar collar }

        NewBead c ->
            let
                colorGen =
                    Random.map (\f -> Color.hsl f 1 0.5) <| Random.float 0 1
            in
            { return
                | collar = Collar.add model.cursor (Collar.beadFromContent c) collar
                , toUndo = Group
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
                        , common = commonUpdate (Delete <| B i) model.common
                    }
            }

        UnpackBead ( w, l ) new ->
            if new then
                { return
                    | collar = Collar.add model.cursor { wheel = w, length = l } collar
                    , toUndo = Do
                    , model = { model | cursor = model.cursor + 1 }
                }

            else
                case model.common.edit of
                    [ B i ] ->
                        update (WheelMsg ( i, Wheel.ChangeContent <| Wheel.getContent { wheel = w } )) ( model, collar )

                    _ ->
                        return

        ResizeToContent i ->
            { return
                | collar =
                    Collar.updateBead i
                        (\b -> { b | length = CommonData.getContentLength <| Wheel.getContent <| Collar.get i collar })
                        collar
                , toUndo = Do
            }

        WheelMsg ( i, subMsg ) ->
            { return | collar = Collar.updateBead i (Wheel.update subMsg) collar, toUndo = Do }

        CommonMsg subMsg ->
            { return | model = { model | common = commonUpdate subMsg model.common } }

        SvgMsg subMsg ->
            { return | model = { model | svg = PanSvg.update subMsg model.svg } }

        SVGSize res ->
            case res of
                Result.Err e ->
                    Debug.log (D.errorToString e) return

                Result.Ok s ->
                    { return
                        | model =
                            { model
                                | svg = PanSvg.update (PanSvg.ScaleSize 1 s) model.svg
                                , common = commonUpdate (PackSvgMsg <| PanSvg.ScaleSize model.common.packScale s) model.common
                            }
                    }

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
    PanSvg.newSVGSize (SVGSize << D.decodeValue PanSvg.sizeDecoder)
        :: (List.map (Sub.map InteractMsg) <| Interact.subs interact)


leftmostPoint : Vec.Vec2
leftmostPoint =
    vec2 0 0


viewContent : ( Model, Colleer ) -> Element Msg
viewContent ( model, collar ) =
    let
        getMod : Int -> Wheel.Mod
        getMod i =
            if model.tool == Edit && model.common.edit == [ B i ] then
                Wheel.Selected False

            else
                case Interact.getInteract model.interact of
                    Just ( IWheel j, Interact.Hover ) ->
                        if B i == j then
                            Wheel.Selectable

                        else
                            Wheel.None

                    _ ->
                        Wheel.None
    in
    Element.html <|
        S.svg
            (List.map (Html.Attributes.map SvgMsg) (PanSvg.svgAttributes model.svg)
                ++ (List.map (Html.Attributes.map InteractMsg) <| Interact.dragSpaceEvents model.interact ZSurface)
            )
        <|
            List.map (Svg.map <| InteractMsg << Interact.map fromWheelInteractable)
                (List.foldl
                    (\b ( l, ( p, i ) ) ->
                        ( Wheel.view b.wheel
                            (vec2 (p + b.length / 2) <| Vec.getY leftmostPoint)
                            b.length
                            { defaultStyle | mod = getMod i }
                            (B i)
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
        [ SA.x <| Num <| Collar.getCumulLengthAt cursor c - cursorW / 2
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
    case model.mode of
        CommonMode (ChangeSound id) ->
            viewDetailChangingSound id (Content.C c) <| ChangedMode <| CommonMode Normal

        _ ->
            case ( model.tool, model.common.edit ) of
                ( Edit, [ B i ] ) ->
                    let
                        b =
                            Collar.get i c
                    in
                    [ viewDetailsColumn <|
                        [ viewNameInput b (Collar.toUID i) <| \str -> WheelMsg ( i, Wheel.Named str )
                        , viewContentButton b <| OutMsg <| Inside <| B i
                        , viewVolumeSlider b <| \f -> WheelMsg ( i, Wheel.ChangeVolume f )
                        , viewResizeToInsideLength <| ResizeToContent i
                        , viewChangeContent <| ChangedMode <| CommonMode <| ChangeSound <| B i
                        , viewDeleteButton <| DeleteBead i
                        ]
                            ++ viewPackButtons model.common (Content.C c) (\w -> UnpackBead ( w, 0 ) False) CommonMsg
                    ]

                _ ->
                    []


manageInteractEvent : Interact.Event Interactable Zone -> Model -> Colleer -> Return
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
            { return | outMsg = interactNav event <| Content.C collar }

        CommonMode (ChangeSound (B i)) ->
            case ( event.item, event.action ) of
                ( ISound s, Interact.Clicked _ ) ->
                    update (WheelMsg ( i, Wheel.ChangeContent <| Content.S s ))
                        ( { model | mode = CommonMode Normal }, collar )

                _ ->
                    return

        CommonMode SupprMode ->
            case ( event.item, event.action ) of
                ( IWheel (B id), Interact.Clicked _ ) ->
                    update (DeleteBead id) ( model, collar )

                ( IWheel (P id), Interact.Clicked _ ) ->
                    update (CommonMsg <| Unpack id) ( model, collar )

                _ ->
                    return

        CommonMode Normal ->
            case ( event.item, event.action ) of
                ( ISound s, Interact.Clicked _ ) ->
                    update (NewBead <| Content.S s) ( model, collar )

                ( IWheel (P id), Interact.Clicked _ ) ->
                    let
                        p =
                            Coll.get id model.common.pack
                    in
                    update (UnpackBead ( p.wheel, p.length ) True) ( model, collar )

                _ ->
                    case model.tool of
                        Play on ->
                            --TODO Factorize
                            let
                                scale =
                                    PanSvg.getScale model.svg
                            in
                            case ( event.item, event.action ) of
                                -- MUTE
                                ( IWheel (B i), Interact.Clicked _ ) ->
                                    let
                                        w =
                                            (Collar.get i collar).wheel

                                        newMute =
                                            not w.mute
                                    in
                                    { return
                                        | collar = Collar.updateBead i (\b -> { b | wheel = { w | mute = newMute } }) collar
                                        , toUndo = Do
                                        , toEngine = Just <| Engine.mutedBead i newMute
                                    }

                                _ ->
                                    return

                        Edit ->
                            { return | model = { model | common = interactSelectEdit event model.common } }

        _ ->
            return
