module Utils.Interact exposing (..)

import Browser.Events as BE
import Html
import Html.Attributes
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as D
import Math.Vector2 as Vec exposing (Vec2, vec2)
import Time exposing (Posix)


holdTime : Float
holdTime =
    500


type alias Interact item =
    Maybe ( item, Mode )


type Mode
    = Hover
    | Click
    | Hold
    | Drag


getInteract : State item zone -> Interact item
getInteract (S s) =
    case ( s.hover, s.click ) of
        ( Just item, Nothing ) ->
            Just ( item, Hover )

        ( _, Just { item, hold } ) ->
            case hold of
                Moving _ ->
                    Just ( item, Drag )

                Holding ->
                    Just ( item, Hold )

                Clicking ->
                    Just ( item, Click )

        _ ->
            Nothing


type State item zone
    = S
        { hover : Maybe item
        , click : Maybe (ClickState item zone)
        }


type alias ClickState item zone =
    { item : item
    , pos : Vec2
    , abs : Vec2
    , hold : HoldState zone
    , keys : Mouse.Keys
    , startTime : Int
    }


type HoldState zone
    = Clicking
    | Holding
    | Moving ( Vec2, zone )


init : State item zone
init =
    S
        { hover = Nothing
        , click = Nothing
        }


type Msg item zone
    = HoverIn item
    | HoverOut
    | StartClick item Vec2 Vec2 Mouse.Keys Int -- offsetPos clientPos
    | ClickMove zone Vec2 Vec2
    | ClickHold
    | EndClick Int
    | AbortClick
    | NOOP


map : (a -> b) -> Msg a c -> Msg b c
map f m =
    case m of
        HoverIn a ->
            HoverIn (f a)

        StartClick a v c k t ->
            StartClick (f a) v c k t

        HoverOut ->
            HoverOut

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


type alias Event item zone =
    { action : Action zone
    , item : item
    }


type Action zone
    = Clicked ( Bool, Bool, Bool )
    | Dragged (DragInfo zone) zone ( Bool, Bool, Bool ) -- Shift Ctrl Alt
    | DragIn
    | DragOut
    | DragEnded Bool -- True for Up, False for Abort
    | Start Vec2
    | Holded
    | HoldEnded Int -- milliseconds


type alias DragInfo zone =
    { start : ( Vec2, zone )
    , oldPos : Vec2
    , newPos : Vec2
    , startD : Vec2
    , absD : Vec2
    }


update :
    Msg item zone
    -> State item zone
    -> ( State item zone, Maybe (Event item zone) )
update msg (S state) =
    case msg of
        HoverIn id ->
            ( S { state | hover = Just id }
            , Maybe.map (always (Event DragIn id)) state.click
            )

        HoverOut ->
            ( S { state | hover = Nothing }
            , Maybe.map2 (always (\hover -> Event DragIn hover))
                state.click
                state.hover
            )

        StartClick id pos abs keys time ->
            ( S
                { state
                    | click =
                        Just
                            { item = id
                            , pos = pos
                            , abs = abs
                            , hold = Clicking
                            , keys = keys
                            , startTime = time
                            }
                }
            , Just { item = id, action = Start pos }
            )

        ClickMove zone pos abs ->
            case state.click of
                Just click ->
                    let
                        dragInit =
                            case click.hold of
                                Moving res ->
                                    res

                                _ ->
                                    ( click.pos, zone )
                    in
                    ( S
                        { state
                            | click =
                                Just
                                    { click
                                        | pos = pos
                                        , abs = abs
                                        , hold = Moving dragInit
                                    }
                        }
                    , Just <|
                        Event
                            (Dragged
                                { start = dragInit
                                , oldPos = click.pos
                                , newPos = pos
                                , startD = Vec.sub abs click.abs
                                , absD = Vec.sub abs click.abs
                                }
                                zone
                             <|
                                tupleFromKeys click.keys
                            )
                            click.item
                    )

                _ ->
                    ( S state, Nothing )

        ClickHold ->
            case state.click of
                Just click ->
                    ( S { state | click = Just { click | hold = Holding } }
                    , Just <| Event Holded click.item
                    )

                _ ->
                    ( S state, Nothing )

        EndClick time ->
            case state.click of
                Just { item, hold, keys, startTime } ->
                    ( S { state | click = Nothing }
                    , case hold of
                        Moving _ ->
                            Just <| Event (DragEnded True) item

                        Holding ->
                            Just <| Event (HoldEnded (time - startTime)) item

                        Clicking ->
                            Just <| Event (Clicked <| tupleFromKeys keys) item
                    )

                _ ->
                    ( S state, Nothing )

        AbortClick ->
            case state.click of
                Just { item, hold, keys } ->
                    ( S { state | click = Nothing }
                    , case hold of
                        Moving _ ->
                            Just <| Event (DragEnded False) item

                        Holding ->
                            Just <| Event (HoldEnded 0) item

                        Clicking ->
                            Nothing
                    )

                _ ->
                    ( S state, Nothing )

        NOOP ->
            ( S state, Nothing )


sub : State item zone -> Sub (Msg item zone)
sub (S { click }) =
    (case click of
        Nothing ->
            []

        Just { hold } ->
            [ BE.onMouseUp <|
                D.map (EndClick << round) <|
                    D.field "timeStamp" D.float
            , BE.onVisibilityChange
                (\v ->
                    -- TODO check bug visibility hidden not emitted on window change but on tab change
                    Debug.log (Debug.toString v) <|
                        case v of
                            BE.Hidden ->
                                AbortClick

                            _ ->
                                NOOP
                )
            ]
                ++ (case hold of
                        Clicking ->
                            [ Time.every holdTime <| always ClickHold ]

                        _ ->
                            []
                   )
    )
        |> Sub.batch


dragSpaceEvents : State item zone -> zone -> List (Html.Attribute (Msg item zone))
dragSpaceEvents (S { click }) zone =
    case click of
        Nothing ->
            []

        Just _ ->
            [ Mouse.onMove <| \{ offsetPos, clientPos } -> ClickMove zone (vecFromTuple offsetPos) (vecFromTuple clientPos) ]


hoverEvents : item -> List (Html.Attribute (Msg item zone))
hoverEvents id =
    [ Mouse.onEnter <| always <| HoverIn id
    , Mouse.onLeave <| always HoverOut
    ]


draggableEvents : item -> List (Html.Attribute (Msg item zone))
draggableEvents id =
    [ onMouseDowm <|
        \{ e, time } ->
            StartClick id
                (vecFromTuple e.offsetPos)
                (vecFromTuple e.clientPos)
                e.keys
                time
    , Html.Attributes.attribute "class" "draggable"
    ]


type alias TimedEvent =
    { e : Mouse.Event
    , time : Int
    }


decodeWithTime : D.Decoder TimedEvent
decodeWithTime =
    D.map2 TimedEvent
        Mouse.eventDecoder
    <|
        D.map round <|
            D.field "timeStamp" D.float


onMouseDowm : (TimedEvent -> msg) -> Html.Attribute msg
onMouseDowm msg =
    let
        opt m =
            Debug.log "dec" <|
                { message = m
                , stopPropagation = True
                , preventDefault = True
                }

        decoder =
            D.map opt <| D.map msg <| decodeWithTime
    in
    Html.Events.custom "mousedown" decoder



-- MISC


tupleFromKeys : Mouse.Keys -> ( Bool, Bool, Bool )
tupleFromKeys { alt, shift, ctrl } =
    ( shift, ctrl, alt )


vecFromTuple : ( Float, Float ) -> Vec2
vecFromTuple t =
    vec2 (Tuple.first t) (Tuple.second t)
