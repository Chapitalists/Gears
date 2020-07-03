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
    | Repeat String


type Msg
    = HoldDown String
    | HoldUp String


update : Msg -> State -> ( State, List Event )
update msg state =
    case msg of
        HoldDown key ->
            let
                hold =
                    Set.insert key state
            in
            ( hold, [ Hold hold, Repeat key ] )

        HoldUp key ->
            let
                hold =
                    Set.remove key state
            in
            ( hold
            , Hold hold
                :: (if Set.member key state then
                        [ Press key ]

                    else
                        []
                   )
            )


subs : List (Sub Msg)
subs =
    [ BE.onKeyDown <| D.andThen (\str -> D.succeed <| HoldDown str) <| D.field "key" D.string
    , BE.onKeyUp <| D.andThen (\str -> D.succeed <| HoldUp str) <| D.field "key" D.string
    ]
