module Interact exposing (..)

import Browser.Events as BE
import Html
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as D
import Math.Vector2 as Vec exposing (Vec2, vec2)


type alias Interact item =
    Maybe ( item, Mode )


type Mode
    = Hover
    | Click
    | Drag


getInteract : State item zone -> Interact item
getInteract (S state) =
    case ( state.hover, state.click ) of
        ( Just item, Nothing ) ->
            Just ( item, Hover )

        ( _, Just { item, moved } ) ->
            case moved of
                Just _ ->
                    Just ( item, Drag )

                Nothing ->
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
    , moved : Maybe ( Vec2, zone )
    , keys : Mouse.Keys
    }


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


type alias DragInfo zone =
    { init : ( Vec2, zone )
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
            ( S { state | click = Just <| ClickState id pos abs Nothing keys }, Nothing )

        ClickMove zone pos abs ->
            case state.click of
                Just click ->
                    let
                        dragInit =
                            Maybe.withDefault ( click.pos, zone ) click.moved
                    in
                    ( S
                        { state
                            | click =
                                Just { click | pos = pos, abs = abs, moved = Just dragInit }
                        }
                    , Just <|
                        Event
                            (Dragged
                                { init = dragInit
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

        EndClick ->
            case state.click of
                Just { item, moved, keys } ->
                    ( S { state | click = Nothing }
                    , case moved of
                        Just _ ->
                            Just <| Event (DragEnded True) item

                        Nothing ->
                            Just <| Event (Clicked <| tupleFromKeys keys) item
                    )

                _ ->
                    ( S state, Nothing )

        AbortClick ->
            case state.click of
                Just { item, moved, keys } ->
                    ( S { state | click = Nothing }
                    , case moved of
                        Just _ ->
                            Just <| Event (DragEnded False) item

                        Nothing ->
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

        Just _ ->
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
