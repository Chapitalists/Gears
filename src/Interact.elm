module Interact exposing (..)

import Browser.Events as BE
import Html
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as D
import Math.Vector2 as Vec exposing (Vec2, vec2)
import Time


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
    | StartClick item Vec2 Vec2 Mouse.Keys -- offsetPos clientPos
    | ClickMove zone Vec2 Vec2
    | ClickHold
    | EndClick
    | AbortClick
    | NOOP


map : (a -> b) -> Msg a c -> Msg b c
map f m =
    case m of
        HoverIn a ->
            HoverIn (f a)

        StartClick a v c k ->
            StartClick (f a) v c k

        HoverOut ->
            HoverOut

        ClickMove z v c ->
            ClickMove z v c

        ClickHold ->
            ClickHold

        EndClick ->
            EndClick

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
    | Holded
    | HoldEnded


type alias DragInfo zone =
    { start : ( Vec2, zone )
    , oldPos : Vec2
    , newPos : Vec2
    , startD : Vec2
    , absD : Vec2
    }


update : Msg item zone -> State item zone -> ( State item zone, Maybe (Event item zone) )
update msg (S state) =
    case msg of
        HoverIn id ->
            ( S { state | hover = Just id }
            , Maybe.map (always <| Event DragIn id) state.click
            )

        HoverOut ->
            case state.hover of
                Just id ->
                    ( S { state | hover = Nothing }
                    , Maybe.map (always <| Event DragOut id) state.click
                    )

                Nothing ->
                    ( S state, Nothing )

        StartClick id pos abs keys ->
            ( S { state | click = Just <| ClickState id pos abs Clicking keys }, Nothing )

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
                                Just { click | pos = pos, abs = abs, hold = Moving dragInit }
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

        EndClick ->
            case state.click of
                Just { item, hold, keys } ->
                    ( S { state | click = Nothing }
                    , case hold of
                        Moving _ ->
                            Just <| Event (DragEnded True) item

                        Holding ->
                            Just <| Event HoldEnded item

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
                            Just <| Event HoldEnded item

                        Clicking ->
                            Nothing
                    )

                _ ->
                    ( S state, Nothing )

        NOOP ->
            ( S state, Nothing )


subs : State item zone -> List (Sub (Msg item zone))
subs (S { click }) =
    case click of
        Nothing ->
            []

        Just { hold } ->
            [ BE.onMouseUp <| D.succeed <| EndClick
            , BE.onVisibilityChange
                (\v ->
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
    [ Mouse.onWithOptions "mousedown" { stopPropagation = True, preventDefault = False } <|
        \e -> StartClick id (vecFromTuple e.offsetPos) (vecFromTuple e.clientPos) e.keys
    ]



-- MISC


tupleFromKeys : Mouse.Keys -> ( Bool, Bool, Bool )
tupleFromKeys { alt, shift, ctrl } =
    ( shift, ctrl, alt )


vecFromTuple : ( Float, Float ) -> Vec2
vecFromTuple t =
    vec2 (Tuple.first t) (Tuple.second t)
