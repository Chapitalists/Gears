module Engine exposing
    ( Engine
    , addPlaying
    , init
    , isPlaying
    , muted
    , mutedBead
    , playCollar
    , playingIds
    , setPlaying
    , stop
    , volumeBead
    , volumeChanged
    )

import Coll exposing (Coll, Id)
import Data.Collar as Collar exposing (Beed, Colleer)
import Data.Content as Content
import Data.Gear as Gear exposing (Gear)
import Data.Mobile exposing (Geer, Mobeel)
import Data.Wheel as Wheel exposing (Wheel)
import Harmony as Harmo
import Json.Encode as E
import Motor
import Sound


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
        , ( "gears", E.list (encodeGear True coll) els )
        ]


stop : E.Value
stop =
    E.object [ ( "action", E.string "stopReset" ) ]


playCollar : Colleer -> E.Value
playCollar collar =
    E.object
        [ ( "action", E.string "playCollar" )
        , ( "baseId", E.string <| String.dropRight 1 <| Collar.toUID 0 )
        , ( "collar", encodeCollar collar True )
        ]


mutedBead : Int -> Bool -> E.Value
mutedBead i mute =
    E.object
        [ ( "action", E.string "muteBead" )
        , ( "index", E.int i )
        , ( "value", E.bool mute )
        ]


volumeBead : Int -> Float -> E.Value
volumeBead i volume =
    E.object
        [ ( "action", E.string "volumeBead" )
        , ( "index", E.int i )
        , ( "value", E.float <| clamp 0 1 volume )
        ]


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


encodeWheel : Wheel -> Bool -> List ( String, E.Value )
encodeWheel w hasView =
    [ ( "mute", E.bool w.mute )
    , ( "volume", E.float <| clamp 0 1 w.volume )
    , ( "startPercent", E.float w.startPercent )
    , ( "view", E.bool hasView )
    ]
        ++ (case Wheel.getWheelContent w of
                Content.S s ->
                    [ ( "soundName", E.string <| Sound.toString s ) ]

                Content.M m ->
                    [ ( "mobile", encodeMobile m False ) ]

                Content.C c ->
                    [ ( "collar", encodeCollar c False ) ]
           )


encodeGear : Bool -> Coll Geer -> Id Geer -> E.Value
encodeGear hasView coll id =
    let
        g =
            Coll.get id coll

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
             ]
                ++ encodeWheel g.wheel hasView
            )


encodeMobile : Mobeel -> Bool -> E.Value
encodeMobile { motor, gears } hasView =
    E.object
        [ ( "length", E.float <| Harmo.getLengthId motor gears )
        , ( "gears", E.list (encodeGear hasView gears) <| Motor.getMotored motor gears )
        ]


encodeCollar : Colleer -> Bool -> E.Value
encodeCollar c hasView =
    E.object
        [ ( "length", E.float <| Collar.getCumulLengthAt c.matrice c )
        , ( "loopStart", E.float c.loop )
        , ( "beads", E.list (encodeBead hasView) <| Collar.getBeads c )
        ]


encodeBead : Bool -> Beed -> E.Value
encodeBead hasView b =
    E.object
        (( "length", E.float b.length )
            :: encodeWheel b.wheel hasView
        )
