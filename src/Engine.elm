module Engine exposing
    ( Engine
    , addPlaying
    , init
    , isPlaying
    , muted
    , playingIds
    , setPlaying
    , stop
    , volumeChanged
    )

import Coll exposing (Coll, Id)
import Content
import Gear exposing (Gear)
import Harmony as Harmo
import Json.Encode as E
import Mobile exposing (Geer, Mobeel)
import Motor
import Sound
import Wheel exposing (Wheel)


type Engine
    = E (List (Id Geer))


init : Engine
init =
    E []


isPlaying : Id Geer -> Engine -> Bool
isPlaying id (E l) =
    List.member id l


playingIds : Engine -> List (Id Geer)
playingIds (E e) =
    e


setPlaying : List (Id Geer) -> Coll Geer -> Engine -> ( Engine, Maybe E.Value )
setPlaying l coll (E e) =
    ( E l
    , if List.isEmpty l then
        Nothing

      else
        Just <| playPause coll <| List.filter (\el -> not <| List.member el l) e
    )


addPlaying : List (Id Geer) -> Coll Geer -> Engine -> ( Engine, Maybe E.Value )
addPlaying l coll (E e) =
    ( E (e ++ l)
    , if List.isEmpty l then
        Nothing

      else
        Just <| playPause coll l
    )


playPause : Coll Geer -> List (Id Geer) -> E.Value
playPause coll els =
    E.object
        [ ( "action", E.string "playPause" )
        , ( "gears", E.list (encodeGear coll) els )
        ]


stop : E.Value
stop =
    E.object [ ( "action", E.string "stopReset" ) ]


muted : Id Geer -> Bool -> Engine -> Maybe E.Value
muted id mute e =
    if isPlaying id e then
        Just <|
            E.object
                [ ( "action", E.string "mute" )
                , ( "gearId", E.string <| Gear.toUID <| Coll.idMap id )
                , ( "value", E.bool mute )
                ]

    else
        Nothing


volumeChanged : Id Geer -> Float -> Engine -> Maybe E.Value
volumeChanged id volume e =
    if isPlaying id e then
        Just <|
            E.object
                [ ( "action", E.string "volume" )
                , ( "gearId", E.string <| Gear.toUID <| Coll.idMap id )
                , ( "value", E.float <| clamp 0 1 volume )
                ]

    else
        Nothing


encodeGear : Coll Geer -> Id Geer -> E.Value
encodeGear coll id =
    let
        g =
            Coll.get id coll

        w =
            g.wheel

        length =
            Harmo.getLength g.harmony coll

        uid =
            Gear.toUID id
    in
    if length == 0 then
        Debug.log (uid ++ "’s length is 0") E.null

    else
        E.object
            ([ ( "id", E.string <| uid )
             , ( "length", E.float length )
             , ( "mute", E.bool w.mute )
             , ( "volume", E.float <| clamp 0 1 w.volume )
             ]
                ++ (case Wheel.getContent g of
                        Content.S s ->
                            [ ( "soundName", E.string <| Sound.toString s ) ]

                        Content.M m ->
                            [ ( "mobile", encodeMobile m ) ]

                        Content.C _ ->
                            Debug.todo "Encode encapsulated"
                   )
            )


encodeMobile : Mobeel -> E.Value
encodeMobile { motor, gears } =
    E.object
        [ ( "motor", encodeGear gears motor )
        , ( "gears", E.list (encodeGear gears) <| Motor.getMotored motor gears )
        ]
