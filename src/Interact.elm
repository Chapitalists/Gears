module Interact exposing (..)

import Browser.Events as BE
import Html
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as D
import Math.Vector2 exposing (Vec2, vec2)


type alias Interact item =
    Maybe ( item, Mode )


type Mode
    = Hover
    | Click
    | Drag


getInteract : State item -> Interact item
getInteract (S state) =
    case ( state.hover, state.click ) of
        ( Just item, Nothing ) ->
            Just ( item, Hover )

        ( _, Just { item, moved } ) ->
            if moved then
                Just ( item, Drag )

            else
                Just ( item, Click )

        _ ->
            Nothing


type State item
    = S
        { hover : Maybe item
        , click : Maybe (ClickState item)
        }


type alias ClickState item =
    { item : item
    , pos : Vec2
    , moved : Bool
    , keys : Mouse.Keys
    }


init : State item
init =
    S
        { hover = Nothing
        , click = Nothing
        }


type Msg item
    = HoverIn item
    | HoverOut
    | StartClick item Vec2 Mouse.Keys
    | ClickMove Vec2
    | EndClick
    | AbortClick
    | NOOP


map : (a -> b) -> Msg a -> Msg b
map f m =
    case m of
        HoverIn a ->
            HoverIn (f a)

        StartClick a v k ->
            StartClick (f a) v k

        HoverOut ->
            HoverOut

        ClickMove v ->
            ClickMove v

        EndClick ->
            EndClick

        AbortClick ->
            AbortClick

        NOOP ->
            NOOP


type alias Event item =
    { action : Action
    , item : item
    }


type Action
    = Clicked ( Bool, Bool, Bool )
    | Dragged Vec2 Vec2 ( Bool, Bool, Bool ) -- oldPos newPos
    | DragIn
    | DragOut
    | DragEnded Bool -- True for Up, False for Abort


update : Msg item -> State item -> ( State item, Maybe (Event item) )
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

        StartClick id pos keys ->
            ( S { state | click = Just <| ClickState id pos False keys }, Nothing )

        ClickMove pos ->
            case state.click of
                Just click ->
                    ( S { state | click = Just { click | pos = pos, moved = True } }
                    , Just <| Event (Dragged click.pos pos <| tupleFromKeys click.keys) click.item
                    )

                _ ->
                    ( S state, Nothing )

        EndClick ->
            case state.click of
                Just { item, moved, keys } ->
                    ( S { state | click = Nothing }
                    , if moved then
                        Just <| Event (DragEnded True) item

                      else
                        Just <| Event (Clicked <| tupleFromKeys keys) item
                    )

                _ ->
                    ( S state, Nothing )

        AbortClick ->
            case state.click of
                Just { item, moved, keys } ->
                    ( S { state | click = Nothing }
                    , if moved then
                        Just <| Event (DragEnded False) item

                      else
                        Nothing
                    )

                _ ->
                    ( S state, Nothing )

        NOOP ->
            ( S state, Nothing )


subs : State item -> List (Sub (Msg item))
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


dragSpaceEvents : State item -> List (Html.Attribute (Msg item))
dragSpaceEvents (S { click }) =
    case click of
        Nothing ->
            []

        Just _ ->
            [ Mouse.onMove <| ClickMove << vecFromTuple << .offsetPos ]


hoverEvents : item -> List (Html.Attribute (Msg item))
hoverEvents id =
    [ Mouse.onEnter <| always <| HoverIn id
    , Mouse.onLeave <| always HoverOut
    ]


draggableEvents : item -> List (Html.Attribute (Msg item))
draggableEvents id =
    [ Mouse.onWithOptions "mousedown" { stopPropagation = True, preventDefault = False } <|
        \e -> StartClick id (vecFromTuple e.offsetPos) e.keys
    ]



-- MISC


tupleFromKeys : Mouse.Keys -> ( Bool, Bool, Bool )
tupleFromKeys { alt, shift, ctrl } =
    ( shift, ctrl, alt )


vecFromTuple : ( Float, Float ) -> Vec2
vecFromTuple t =
    vec2 (Tuple.first t) (Tuple.second t)
