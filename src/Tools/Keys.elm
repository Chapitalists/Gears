module Tools.Keys exposing (..)

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
        HoldDown code ->
            let
                hold =
                    Set.insert code state
            in
            ( hold, [ Hold hold, Repeat code ] )

        HoldUp code ->
            let
                hold =
                    Set.remove code state
            in
            ( hold
            , Hold hold
                :: (if Set.member code state then
                        [ Press code ]

                    else
                        []
                   )
            )


sub : Sub Msg
sub =
    [ BE.onKeyDown <|
        D.andThen
            (\node ->
                if node == "INPUT" || node == "TEXTAREA" then
                    D.fail "ignored from text input"

                else
                    D.andThen
                        (\str -> D.succeed <| HoldDown str)
                    <|
                        D.field "code" D.string
            )
        <|
            D.at [ "target", "nodeName" ] D.string
    , BE.onKeyUp <| D.andThen (\str -> D.succeed <| HoldUp str) <| D.field "code" D.string
    ]
        |> Sub.batch
