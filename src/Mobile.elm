module Mobile exposing (..)

import Coll exposing (Coll, Id)
import Content exposing (Content, Mobile)
import Gear exposing (Gear)
import Harmony as Harmo exposing (Harmony)
import Json.Decode as D
import Json.Encode as E
import Math.Vector2 exposing (Vec2)
import Sound exposing (Sound)
import Wheel exposing (Wheel)



--TODO Put here half Editor.Mobile, let there only interaction and tools view
--TODO Would lead to separate View and Controller, anti Elm Architecture
--TODO Should find another way to split these two files?


type alias Mobeel =
    Mobile Wheel


type alias Geer =
    Gear Wheel


new : Mobeel
new =
    { motor = Coll.startId, gears = Coll.empty Gear.typeString defaultGear }


fromGear : Geer -> Mobeel
fromGear g =
    let
        ( id, coll ) =
            Coll.insertTellId g <| Coll.empty Gear.typeString defaultGear
    in
    { motor = id, gears = coll }


defaultGear : Geer
defaultGear =
    Gear.default Wheel.default


gearFromContent : Content Wheel -> Vec2 -> Geer
gearFromContent c pos =
    let
        length =
            case c of
                Content.S s ->
                    Sound.length s

                Content.M m ->
                    Harmo.getLengthId m.motor m.gears

                Content.C col ->
                    col.matrice
    in
    { pos = pos
    , harmony = Harmo.newSelf length
    , motor = []
    , wheel = Wheel.fromContent c
    }


gearName : Id Geer -> Coll Geer -> String
gearName id coll =
    let
        name =
            (Coll.get id coll).wheel.name
    in
    if String.isEmpty name then
        Gear.toUID id

    else
        name


gearPosSize : Id Geer -> Coll Geer -> ( Vec2, Float )
gearPosSize id coll =
    let
        g =
            Coll.get id coll
    in
    ( g.pos, Harmo.getLength g.harmony coll )


encoder : Mobeel -> E.Value
encoder =
    Content.mobileEncoder Wheel.encoder


decoder : D.Decoder Mobeel
decoder =
    Content.mobileDecoder Wheel.decoder Wheel.default
