port module Engine exposing (Engine, addMotor, getAllLinks, init, isPlaying, mute, rmMotors, toggle)

import Coll exposing (Coll, Id)
import Gear exposing (Gear, getMotors)
import Json.Encode as E
import Link exposing (Link)


port toEngine : E.Value -> Cmd msg



-- TODO Could store adjacency lists (Gear.motors) in Dict (Id Gear, List (Id Gear))
-- but as need for Sets with Ids, either deopacify or add this kind of Dict into Coll API


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
                playPauseLinked motor gears
        in
        ( E { e | playing = addPlaying }, cmd )


addMotor : Link -> Coll Gear -> Engine -> ( Coll Gear, Engine, Cmd msg )
addMotor l gears (E e) =
    let
        ( addPlaying, cmd ) =
            case ( List.member (Tuple.first l) e.playing, List.member (Tuple.second l) e.playing ) of
                ( True, False ) ->
                    playPauseLinked (Tuple.second l) gears

                ( False, True ) ->
                    playPauseLinked (Tuple.first l) gears

                _ ->
                    ( [], Cmd.none )
    in
    ( Gear.addMotorLink l gears
    , E { e | playing = e.playing ++ addPlaying }
    , cmd
    )


rmMotors : List Link -> { a | gears : Coll Gear, motor : Id Gear } -> Engine -> ( Coll Gear, Engine, Cmd msg )
rmMotors ls { gears, motor } (E e) =
    let
        newGears =
            List.foldl Gear.rmMotorLink gears ls
    in
    if isPlaying (E e) then
        let
            motored =
                visitMotors newGears motor []
        in
        ( newGears
        , E { e | playing = motored }
        , playPause (List.filter (\el -> not <| List.member el motored) e.playing) gears
        )

    else
        ( newGears, E e, Cmd.none )


mute : Id Gear -> Coll Gear -> Engine -> ( Coll Gear, Cmd msg )
mute id gears (E e) =
    let
        g =
            Coll.get id gears

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


playPauseLinked : Id Gear -> Coll Gear -> ( List (Id Gear), Cmd msg )
playPauseLinked motor gears =
    let
        changed =
            visitMotors gears motor []
    in
    ( changed
    , playPause changed gears
    )


playPause : List (Id Gear) -> Coll Gear -> Cmd msg
playPause ids gears =
    toEngine <|
        E.object
            [ ( "action", E.string "playPause" )
            , ( "gears", E.list (\id -> Gear.encoder id gears) ids )
            ]


stop : Cmd msg
stop =
    toEngine <| E.object [ ( "action", E.string "stopReset" ) ]


getAllLinks : Coll Gear -> List Link
getAllLinks gears =
    Coll.ids gears
        |> List.foldl (\id -> visitToLinks gears id Nothing) ( [], [] )
        |> Tuple.second



-- TODO sometimes double Link in the list
-- either Set Link or switch from adjacency list to edge list ?


visitToLinks : Coll Gear -> Id Gear -> Maybe (Id Gear) -> ( List (Id Gear), List Link ) -> ( List (Id Gear), List Link )
visitToLinks gears motorId mayFromId ( visited, links ) =
    if List.member motorId visited then
        ( visited, links )

    else
        let
            g =
                Coll.get motorId gears
        in
        getMotors g
            |> List.foldl
                (\neighbour ( v, l ) ->
                    if Just neighbour == mayFromId then
                        ( v, l )

                    else
                        visitToLinks gears neighbour (Just motorId) ( v, ( motorId, neighbour ) :: l )
                )
                ( motorId :: visited, links )


visitMotors : Coll Gear -> Id Gear -> List (Id Gear) -> List (Id Gear)
visitMotors gears motorId visited =
    if List.member motorId visited then
        visited

    else
        let
            g =
                Coll.get motorId gears
        in
        getMotors g
            |> List.foldl
                (\neighbour -> visitMotors gears neighbour)
                (motorId :: visited)
