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

        ( _, Just ( item, _ ) ) ->
            if state.moved then
                Just ( item, Drag )

            else
                Just ( item, Click )

        _ ->
            Nothing


type State item
    = S
        { hover : Maybe item
        , click : Maybe ( item, Vec2 )
        , moved : Bool
        }


init : State item
init =
    S { hover = Nothing, click = Nothing, moved = False }


type Msg item
    = HoverIn item
    | HoverOut
    | StartClick item Vec2
    | ClickMove Vec2
    | EndClick
    | AbortClick
    | NOOP


type Event item
    = NoEvent
    | Moved item Vec2 Vec2 -- old new
    | Clicked item


update : Msg item -> State item -> ( State item, Event item )
update msg (S state) =
    case msg of
        HoverIn id ->
            ( S { state | hover = Just id }, NoEvent )

        HoverOut ->
            ( S { state | hover = Nothing }, NoEvent )

        StartClick id pos ->
            ( S { state | click = Just ( id, pos ), moved = False }, NoEvent )

        ClickMove pos ->
            case state.click of
                Just ( item, oldPos ) ->
                    ( S { state | click = Just ( item, pos ), moved = True }, Moved item oldPos pos )

                _ ->
                    Debug.log "IMPOSSIBLE ClickMove without state.pos or state.click" ( S state, NoEvent )

        EndClick ->
            ( S { state | click = Nothing }
            , if state.moved then
                NoEvent

              else
                case state.click of
                    Just ( item, _ ) ->
                        Clicked item

                    _ ->
                        Debug.log "IMPOSSIBLE EndClick without state.click" NoEvent
            )

        AbortClick ->
            ( S { state | click = Nothing }, NoEvent )

        NOOP ->
            ( S state, NoEvent )


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
            [ Mouse.onMove <| ClickMove << vecFromTuple << .clientPos ]


hoverEvents : Bool -> item -> List (Html.Attribute (Msg item))
hoverEvents hover id =
    [ Mouse.onEnter <| always <| HoverIn id ]
        ++ (if hover then
                [ Mouse.onLeave <| always HoverOut ]

            else
                []
           )


draggableEvents : item -> List (Html.Attribute (Msg item))
draggableEvents id =
    [ Mouse.onDown <| StartClick id << vecFromTuple << .clientPos ]



-- MISC


vecFromTuple : ( Float, Float ) -> Vec2
vecFromTuple t =
    vec2 (Tuple.first t) (Tuple.second t)
