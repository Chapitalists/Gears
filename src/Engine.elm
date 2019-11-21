port module Engine exposing (..)

import Coll exposing (Coll, Id)
import Gear exposing (Gear, getMotors)
import Json.Encode as E
import Link exposing (Link)


port toEngine : E.Value -> Cmd msg



--TODO in case of cycle, should know which Gear is playing or not to prevent pausing


type Engine
    = E { play : Bool }


init : Engine
init =
    E { play = False }


isPlaying : Engine -> Bool
isPlaying (E e) =
    e.play


toggle : { a | gears : Coll Gear, motor : Id Gear } -> Engine -> ( Engine, Cmd msg )
toggle { gears, motor } (E e) =
    let
        newPlay =
            not e.play
    in
    ( E { e | play = newPlay }
    , if newPlay then
        playPause motor gears

      else
        stop
    )


addMotor : Link -> Coll Gear -> Engine -> ( Coll Gear, Cmd msg )
addMotor l gears (E e) =
    ( Gear.addMotorLink l gears
    , if e.play then
        -- TODO
        Cmd.none

      else
        Cmd.none
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
            , if not e.play then
                Cmd.none

              else
                toEngine <|
                    E.object
                        [ ( "action", E.string "mute" )
                        , ( "gearId", E.string <| Gear.toUID id )
                        , ( "value", E.bool newMute )
                        ]
            )


playPause : Id Gear -> Coll Gear -> Cmd msg
playPause motor gears =
    toEngine <|
        E.object
            [ ( "action", E.string "playPause" )
            , ( "gears", E.list (\id -> Gear.encoder id gears) <| visitMotors gears motor [] )
            ]


stop : Cmd msg
stop =
    toEngine <| E.object [ ( "action", E.string "stopReset" ) ]


visitMotors : Coll Gear -> Id Gear -> List (Id Gear) -> List (Id Gear)
visitMotors gears motorId visited =
    case Coll.get motorId gears of
        Nothing ->
            Gear.debugGear motorId "Gear not found to play" []

        Just g ->
            if List.isEmpty <| getMotors g then
                [ motorId ]

            else
                motorId
                    :: (List.foldl
                            (\neighbour acc ->
                                if List.member neighbour acc then
                                    acc

                                else
                                    visitMotors gears neighbour acc
                            )
                            visited
                        <|
                            getMotors g
                       )
