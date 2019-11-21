port module Engine exposing (..)

import Coll exposing (Coll, Id)
import Gear exposing (Gear, getMotors)
import Json.Encode as E
import Link exposing (Link)


port toEngine : E.Value -> Cmd msg


type Engine
    = E { playing : List (Id Gear) }


init : Engine
init =
    E { playing = [] }


isPlaying : Engine -> Bool
isPlaying (E e) =
    not <| List.isEmpty e.playing


toggle : { a | gears : Coll Gear, motor : Id Gear } -> Engine -> ( Engine, Cmd msg )
toggle { gears, motor } (E e) =
    if isPlaying (E e) then
        ( init, stop )

    else
        let
            ( addPlaying, cmd ) =
                playPause motor gears
        in
        ( E { e | playing = addPlaying }, cmd )


addMotor : Link -> Coll Gear -> Engine -> ( Coll Gear, Engine, Cmd msg )
addMotor l gears (E e) =
    let
        ( addPlaying, cmd ) =
            case ( List.member (Tuple.first l) e.playing, List.member (Tuple.second l) e.playing ) of
                ( True, False ) ->
                    playPause (Tuple.second l) gears

                ( False, True ) ->
                    playPause (Tuple.first l) gears

                _ ->
                    ( [], Cmd.none )
    in
    ( Gear.addMotorLink l gears
    , E { e | playing = e.playing ++ addPlaying }
    , cmd
    )


mute : Id Gear -> Coll Gear -> Engine -> ( Coll Gear, Cmd msg )
mute id gears (E e) =
    case Coll.get id gears of
        Nothing ->
            ( gears, Cmd.none )

        Just g ->
            let
                newMute =
                    not <| Gear.getMute g
            in
            ( Coll.update id (Gear.setMute newMute) gears
            , if isPlaying (E e) then
                Cmd.none

              else
                toEngine <|
                    E.object
                        [ ( "action", E.string "mute" )
                        , ( "gearId", E.string <| Gear.toUID id )
                        , ( "value", E.bool newMute )
                        ]
            )


playPause : Id Gear -> Coll Gear -> ( List (Id Gear), Cmd msg )
playPause motor gears =
    let
        changed =
            visitMotors gears motor []
    in
    ( changed
    , toEngine <|
        E.object
            [ ( "action", E.string "playPause" )
            , ( "gears", E.list (\id -> Gear.encoder id gears) changed )
            ]
    )


stop : Cmd msg
stop =
    toEngine <| E.object [ ( "action", E.string "stopReset" ) ]


getAllLinks : Coll Gear -> List Link
getAllLinks gears =
    Coll.ids gears
        |> List.foldl (\id -> visitToLinks gears id) ( [], [] )
        |> Tuple.second


visitToLinks : Coll Gear -> Id Gear -> ( List (Id Gear), List Link ) -> ( List (Id Gear), List Link )
visitToLinks gears motorId ( visited, links ) =
    if List.member motorId visited then
        ( visited, links )

    else
        case Coll.get motorId gears of
            Nothing ->
                Gear.debugGear motorId "Gear not found to visit motors" ( visited, links )

            Just g ->
                getMotors g
                    |> List.foldl
                        (\neighbour ( v, l ) -> visitToLinks gears neighbour ( v, ( motorId, neighbour ) :: links ))
                        ( motorId :: visited, links )


visitMotors : Coll Gear -> Id Gear -> List (Id Gear) -> List (Id Gear)
visitMotors gears motorId visited =
    if List.member motorId visited then
        visited

    else
        case Coll.get motorId gears of
            Nothing ->
                Gear.debugGear motorId "Gear not found to visit motors" visited

            Just g ->
                getMotors g
                    |> List.foldl
                        (\neighbour -> visitMotors gears neighbour)
                        (motorId :: visited)
