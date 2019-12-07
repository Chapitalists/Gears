module Mobile exposing (..)

import Coll exposing (Coll, Id)
import Content exposing (Mobile)
import Gear exposing (Gear)
import Harmony as Harmo exposing (Harmony)
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Math.Vector2 exposing (Vec2)
import Sound exposing (Sound)
import Wheel exposing (Wheel)



--TODO Put here half Editor.Mobile, let there only interaction and tools view


type alias Mobeel =
    Mobile Wheel


type alias Geer =
    Gear Wheel


defaultGear : Geer
defaultGear =
    Gear.default Wheel.default


fromSound : Sound -> Vec2 -> Geer
fromSound s pos =
    { pos = pos
    , harmony = Harmo.newSelf <| Sound.length s
    , motor = []
    , wheel = Wheel.fromSound s
    }


encoder : Mobeel -> E.Value
encoder m =
    E.object
        [ ( "motor", Coll.idEncoder m.motor )
        , ( "gears"
          , Coll.encoder m.gears <| Gear.encoder Wheel.encoder
          )
        ]


decoder : D.Decoder Mobeel
decoder =
    D.succeed Mobile
        |> required "motor" Coll.idDecoder
        |> required "gears" (Coll.decoder (Gear.decoder Wheel.decoder) Gear.typeString defaultGear)
