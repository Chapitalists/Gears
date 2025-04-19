module Utils.Gesture exposing (..)

import Color exposing (Color)
import Data.Wheel as Wheel exposing (Wheel)
import Html.Attributes as Attr
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as Svg exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Length(..), Opacity(..), Transform(..))
import Utils.Interact as Interact exposing (Action(..))
import Utils.PanSvg as PanSvg exposing (PanSvg)
import Utils.Utils exposing (Size)


dragZoneAngle : Float
dragZoneAngle =
    pi / 8


dragZoneColor : Color
dragZoneColor =
    Color.lightBlue


type Gesture
    = Model Internals


type Msg
    = Msg (Interact.Msg Item Zone)


type Item
    = Item
    | NoItem


type Zone
    = Vertical
    | Horizontal
    | Workplane


type Event
    = Up Float
    | Down Float
    | Left Float
    | Right Float
    | End Bool -- validate


type alias Internals =
    { interact : Interact.State Item Zone
    , touchId : Maybe Int
    }


init =
    Model
        { interact = Interact.init
        , touchId = Nothing
        }


type alias Return =
    { gesture : Gesture
    , event : Maybe Event
    , interactEvent : Maybe (Interact.Event Item Zone)
    , cmd : Cmd Msg
    }


update : Gesture -> Msg -> Return
update (Model model) (Msg msg) =
    let
        ( state, mayEvent, cmd ) =
            Interact.update msg model.interact

        return ( mayId, out ) =
            { gesture = Model { interact = state, touchId = mayId }
            , event = out
            , cmd = cmd
            , interactEvent = mayEvent
            }

        returnEvent id event out =
            if id /= event.touchId then
                return ( Just id, Nothing )

            else
                return ( Just id, Just out )

        endEvent id event out =
            if id /= event.touchId then
                return ( Just id, Nothing )

            else
                return ( Nothing, Just out )
    in
    case mayEvent of
        Nothing ->
            return ( model.touchId, Nothing )

        Just event ->
            case ( model.touchId, event.action, event.item ) of
                ( Nothing, Holded pos, Item ) ->
                    let
                        _ =
                            Debug.log "hold" pos
                    in
                    return ( Just event.touchId, Nothing )

                ( Just id, Dragged info zone _, _ ) ->
                    returnEvent id event <|
                        case zone of
                            Horizontal ->
                                let
                                    diff =
                                        getX info.newPos
                                            - (getX <|
                                                Tuple.first info.start
                                              )
                                in
                                if diff > 0 then
                                    Right diff

                                else
                                    Left -diff

                            Vertical ->
                                let
                                    diff =
                                        getY info.newPos
                                            - (getY <|
                                                Tuple.first info.start
                                              )
                                in
                                if diff > 0 then
                                    Up -diff

                                else
                                    Down diff

                ( Just id, DragEnded bool ) ->
                ( Just id, DragEnded bool, _ ) ->
                    endEvent id event <| End bool

                ( Just id, HoldEnded _, _ ) ->
                    endEvent id event <| End False

                _ ->
                    return ( model.touchId, Nothing )


view : Gesture -> PanSvg -> Size -> Wheel -> List (Svg Msg)
view (Model model) svg { width, height } w =
    case model.touchId of
        Nothing ->
            []

        Just _ ->
            let
                vecP =
                    Wheel.getPos w

                scale =
                    toFloat (max width height) * PanSvg.getScale svg

                x =
                    scale * cos dragZoneAngle

                y =
                    scale * sin dragZoneAngle

                zone z p1 p2 =
                    S.polygon
                        ([ SA.transform
                            [ Translate (Vec2.getX vecP) (Vec2.getY vecP) ]
                         , SA.fill <| Fill dragZoneColor
                         , SA.strokeWidth <| Num 0
                         , SA.points [ ( 0, 0 ), p1, p2 ]
                         ]
                            ++ Interact.dragSpaceEvents z
                         --++ Interact.dragTargetEvents Item
                        )
                        []
            in
            [ Svg.map Msg <|
                S.g [ SA.opacity <| Opacity 0.5 ]
                    [ zone Horizontal ( x, y ) ( x, -y )
                    , zone Horizontal ( -x, y ) ( -x, -y )
                    , zone Vertical ( y, x ) ( -y, x )
                    , zone Vertical ( y, -x ) ( -y, -x )
                    ]
            ]


sub : Gesture -> Sub Msg
sub (Model model) =
    Sub.map Msg <| Interact.sub model.interact


attributes : List (Svg.Attribute Msg)
attributes =
    List.map (Attr.map Msg) <| Interact.draggableEvents Item
