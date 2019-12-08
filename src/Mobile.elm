module Mobile exposing (..)

import Coll exposing (Coll, Id)
import Content exposing (Mobile)
import Gear exposing (Gear)
import Harmony as Harmo exposing (Harmony)
import Json.Decode as D
import Json.Encode as E
import Math.Vector2 exposing (Vec2)
import Sound exposing (Sound)
import Wheel exposing (Wheel)



--TODO Put here half Editor.Mobile, let there only interaction and tools view


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


gearFromSound : Sound -> Vec2 -> Geer
gearFromSound s pos =
    { pos = pos
    , harmony = Harmo.newSelf <| Sound.length s
    , motor = []
    , wheel = Wheel.fromSound s
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


encoder : Mobeel -> E.Value
encoder =
    Content.mobileEncoder Wheel.encoder


decoder : D.Decoder Mobeel
decoder =
    Content.mobileDecoder Wheel.decoder Wheel.default
