module Utils.Interact exposing (..)

import Dict exposing (Dict)
import Html
import Html.Attributes
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as D
import Math.Vector2 as Vec exposing (Vec2, vec2)
import Time exposing (Posix)
import Utils.Utils exposing (unmaybeMap)


interactMinTime : Float
interactMinTime =
    15


tapMaxTime : Float
tapMaxTime =
    300


movePixelThreshold : Float
movePixelThreshold =
    20


holdTime : Float
holdTime =
    500



--type alias Interact item =
--    Maybe ( item, Mode )
--
--
--type Mode
--    = Hover
--    | Click
--    | Hold
--    | Drag
--getInteract : State item zone -> Interact item
--getInteract (S s) =
--    case ( s.hover, s.touch ) of
--        ( Just item, Nothing ) ->
--            Just ( item, Hover )
--
--        ( _, Just { item, hold } ) ->
--            case hold of
--                Moving _ ->
--                    Just ( item, Drag )
--
--                Holding ->
--                    Just ( item, Hold )
--
--                Clicking ->
--                    Just ( item, Click )
--
--        _ ->
--            Nothing


type State item zone
    = S (Dict Int (ClickInfos item zone))


type alias ClickInfos item zone =
    { item : item
    , over : Maybe item
    , pos : Vec2
    , abs : Vec2
    , state : ClickState zone
    , keys : Mouse.Keys
    , startPos : Vec2
    , startAbs : Vec2
    , startTime : Float
    }


type ClickState zone
    = Clicking
    | Holding
    | Moving zone -- startZone



--| PreClicking


init : State item zone
init =
    S Dict.empty


type alias Msg item zone =
    ( Int, BaseEvent item zone )


type BaseEvent item zone
    = Enter item
    | Leave
    | StartClick item Vec2 Vec2 Mouse.Keys Float -- offsetPos clientPos
    | ClickMove zone Vec2 Vec2
    | ClickHold
    | EndClick Float
    | AbortClick
    | NOOP


map : (a -> b) -> Msg a c -> Msg b c
map f ( i, m ) =
    ( i
    , case m of
        Enter a ->
            Enter (f a)

        Leave ->
            Leave

        StartClick a v c k t ->
            StartClick (f a) v c k t

        ClickMove z v c ->
            ClickMove z v c

        ClickHold ->
            ClickHold

        EndClick t ->
            EndClick t

        AbortClick ->
            AbortClick

        NOOP ->
            NOOP
    )


type alias Event item zone =
    { action : Action zone
    , item : item
    , touchId : Int
    }


type Action zone
    = Clicked ( Bool, Bool, Bool )
    | Dragged (DragInfo zone) zone ( Bool, Bool, Bool ) -- Shift Ctrl Alt
    | DragIn
    | DragOut
    | DragEnded Bool -- True for Up, False for Abort
      --| Start Vec2
    | Holded Vec2
    | HoldEnded Float -- milliseconds


type alias DragInfo zone =
    { start : ( Vec2, zone )
    , oldPos : Vec2
    , newPos : Vec2
    , startD : Vec2 -- ??
    , absD : Vec2 -- ??
    }


update :
    Msg item zone
    -> State item zone
    -> ( State item zone, Maybe (Event item zone) )
update ( id, msg ) (S touches) =
    let
        mayTouch =
            Dict.get id touches

        return ( mayT, mayE ) =
            ( S
                (case mayT of
                    Just t ->
                        Dict.insert id t touches

                    Nothing ->
                        Dict.remove id touches
                )
            , Maybe.map
                (\e -> { action = e.action, item = e.item, touchId = id })
                mayE
            )

        mayUpdate =
            return << unmaybeMap mayTouch ( Nothing, Nothing )
    in
    case msg of
        --HoverIn item ->
        --    ( S { state | hover = Just item }
        --      --, Maybe.map (always (Event DragIn item)) state.touch
        --    , Nothing
        --    )
        --
        --HoverOut ->
        --    ( S { state | hover = Nothing }
        --      --, Maybe.map2 (always (\hover -> Event DragOut hover))
        --      --    state.touch
        --      --    state.hover
        --    , Nothing
        --    )
        Enter item ->
            Debug.todo "Enter (dragIn, or hoverIn?)"

        Leave ->
            Debug.todo "Leave (dragOut, or hoverOut?)"

        StartClick item pos abs keys time ->
            return
                ( Just
                    { item = item
                    , over = Nothing
                    , pos = pos
                    , abs = abs
                    , state = Clicking
                    , keys = keys
                    , startPos = pos
                    , startAbs = abs
                    , startTime = time
                    }
                , Nothing
                  --Just
                  --    { item = item, action = Start pos }
                )

        ClickMove zone pos abs ->
            mayUpdate
                (\click ->
                    let
                        startZone =
                            case click.state of
                                Moving z ->
                                    z

                                _ ->
                                    zone

                        moveAmount =
                            Debug.log "startDiff" <| Vec.distance click.startPos pos
                    in
                    if moveAmount < movePixelThreshold then
                        ( Just
                            { click
                                | pos = pos
                                , abs = abs
                                , state = click.state
                            }
                        , Nothing
                        )

                    else
                        ( Just
                            { click
                                | pos = pos
                                , abs = abs
                                , state = Moving startZone
                            }
                        , Just <|
                            { item = click.item
                            , action =
                                Dragged
                                    { start = ( click.startPos, startZone )
                                    , oldPos = click.pos
                                    , newPos = pos
                                    , startD = Vec.sub abs click.abs
                                    , absD = Vec.sub abs click.abs
                                    }
                                    zone
                                <|
                                    tupleFromKeys click.keys
                            }
                        )
                )

        ClickHold ->
            mayUpdate
                (\click ->
                    ( Just { click | state = Holding }
                    , Just <| { action = Holded click.pos, item = click.item }
                    )
                )

        EndClick time ->
            mayUpdate
                (\{ item, state, keys, startTime } ->
                    ( Nothing
                    , Just
                        { item = item
                        , action =
                            case state of
                                Moving _ ->
                                    DragEnded True

                                Holding ->
                                    HoldEnded (time - startTime)

                                Clicking ->
                                    Clicked <| tupleFromKeys keys
                        }
                    )
                )

        AbortClick ->
            mayUpdate
                (\{ item, state, keys } ->
                    ( Nothing
                    , case state of
                        Moving _ ->
                            Just <| { action = DragEnded False, item = item }

                        Holding ->
                            Just <| { action = HoldEnded 0, item = item }

                        Clicking ->
                            Nothing
                    )
                )

        NOOP ->
            ( S touches, Nothing )


sub : State item zone -> Sub (Msg item zone)
sub (S touches) =
    Dict.foldl
        (\id { state } subs ->
            case state of
                Clicking ->
                    (Time.every holdTime <| always ( id, ClickHold ))
                        :: subs

                _ ->
                    subs
        )
        []
        touches
        |> Sub.batch



--dragSpaceEvents : State item zone -> zone -> List (Html.Attribute (Msg item zone))


dragSpaceEvents : zone -> List (Html.Attribute (Msg item zone))
dragSpaceEvents zone =
    --dragSpaceEvents (S { click }) zone =
    --case click of
    --    Nothing ->
    --        []
    --
    --    Just _ ->
    [ Pointer.onMove <|
        \{ pointer, pointerId } ->
            let
                _ =
                    Debug.log "move" pointerId
            in
            ( pointerId
            , ClickMove zone
                (vecFromTuple pointer.offsetPos)
                (vecFromTuple pointer.clientPos)
            )
    ]


hoverEvents : item -> List (Html.Attribute (Msg item zone))
hoverEvents =
    dragTargetEvents



--[ Mouse.onEnter <| always <| HoverIn item
--, Mouse.onLeave <| always HoverOut
--]


dragTargetEvents : item -> List (Html.Attribute (Msg item zone))
dragTargetEvents item =
    [ Pointer.onEnter <|
        \{ pointer, pointerId } -> ( pointerId, Enter item )
    , Pointer.onLeave <|
        \{ pointer, pointerId } -> ( pointerId, Leave )
    ]


draggableEvents : item -> List (Html.Attribute (Msg item zone))
draggableEvents item =
    [ onPointerDown <|
        \{ e, time } ->
            let
                _ =
                    Debug.log "down" ( e.pointerId, time )
            in
            ( e.pointerId
            , StartClick item
                (vecFromTuple e.pointer.offsetPos)
                (vecFromTuple e.pointer.clientPos)
                e.pointer.keys
                time
            )
    , onPointerUp <|
        \{ e, time } ->
            let
                _ =
                    Debug.log "up" ( e.pointerId, time )
            in
            ( e.pointerId
            , EndClick time
            )
    , onPointerCancel <|
        \e ->
            let
                _ =
                    Debug.log "cancel" e.pointerId
            in
            ( e.pointerId
            , AbortClick
            )
    , Html.Attributes.attribute "class" "draggable"
    , Html.Attributes.attribute "onPointerDown" "lala"

    --"event.target.setPointerCapture(event.pointerId)"
    ]


type alias TimedEvent =
    { e : Pointer.Event
    , time : Float
    }


decodeWithTime : D.Decoder TimedEvent
decodeWithTime =
    D.map2 TimedEvent
        Pointer.eventDecoder
    <|
        D.field "timeStamp" D.float


customOn : String -> (TimedEvent -> msg) -> Html.Attribute msg
customOn event msg =
    let
        opt m =
            { message = m
            , stopPropagation = True
            , preventDefault = True
            }

        decoder =
            D.map opt <| D.map msg <| decodeWithTime
    in
    Html.Events.custom event decoder


onPointerDown : (TimedEvent -> msg) -> Html.Attribute msg
onPointerDown =
    customOn "pointerdown"


onPointerUp : (TimedEvent -> msg) -> Html.Attribute msg
onPointerUp =
    customOn "pointerup"


onPointerCancel : (Pointer.Event -> msg) -> Html.Attribute msg
onPointerCancel =
    Pointer.onCancel



-- MISC


tupleFromKeys : Mouse.Keys -> ( Bool, Bool, Bool )
tupleFromKeys { alt, shift, ctrl } =
    ( shift, ctrl, alt )


vecFromTuple : ( Float, Float ) -> Vec2
vecFromTuple t =
    vec2 (Tuple.first t) (Tuple.second t)
