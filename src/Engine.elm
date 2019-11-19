port module Engine exposing (..)

import Coll exposing (Coll, Id)
import Gear exposing (Gear, getMotors)
import Json.Encode as E


port toEngine : E.Value -> Cmd msg


playPause : Id Gear -> Coll Gear -> Cmd msg
playPause motor gears =
    toEngine <|
        E.object
            [ ( "action", E.string "playPause" )
            , ( "gears", E.list (\id -> Gear.encoder id gears) <| visitMotors gears motor [] )
            ]


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
