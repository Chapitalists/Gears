module Keys exposing (..)

import Browser.Events as BE
import Json.Decode as D
import Set exposing (Set)


type alias State =
    Set String


init : State
init =
    Set.empty


type Event
    = Press String
    | Hold (Set String)


type Msg
    = Pressed String
    | HoldDown String
    | HoldUp String


update : Msg -> State -> ( State, Event )
update msg state =
    case msg of
        Pressed code ->
            ( state, Press code )

        HoldDown code ->
            let
                hold =
                    Set.insert code state
            in
            ( hold, Hold hold )

        HoldUp code ->
            let
                hold =
                    Set.remove code state
            in
            ( hold, Hold hold )


subs : List (Sub Msg)
subs =
    [ BE.onKeyPress <| D.andThen (\str -> D.succeed <| Pressed str) <| D.field "code" D.string
    , BE.onKeyDown <| D.andThen (\str -> D.succeed <| HoldDown str) <| D.field "code" D.string
    , BE.onKeyUp <| D.andThen (\str -> D.succeed <| HoldUp str) <| D.field "code" D.string
    ]
