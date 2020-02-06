module Data.Mobile exposing (..)

import Coll exposing (Coll, Id)
import Data.Common exposing (..)
import Data.Content as Content exposing (Content, Mobile)
import Data.Gear as Gear exposing (Gear)
import Data.Wheel as Wheel exposing (Conteet, Wheel)
import Harmony as Harmo exposing (Harmony)
import Json.Decode as D
import Json.Encode as E
import Math.Vector2 exposing (Vec2)
import Motor



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


gearFromContent : Conteet -> Vec2 -> Geer
gearFromContent c pos =
    { pos = pos
    , harmony = Harmo.newSelf <| getContentLength c
    , motor = []
    , wheel = Wheel.fromContent c
    }


newSizedGear : Vec2 -> Float -> Wheel -> Geer
newSizedGear p l w =
    { pos = p, harmony = Harmo.newSelf l, motor = [], wheel = w }



-- TODO remove and use Common.getName instead


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


rm : Id Geer -> Mobeel -> Mobeel
rm id m =
    if id == m.motor then
        m

    else
        let
            gears =
                Motor.clean id m.gears

            harmo =
                (Coll.get id gears).harmony
        in
        -- TODO check and use harmo clean
        if Harmo.hasHarmonics harmo then
            -- TODO delete base ?
            Debug.log "TODO delete base" m

        else
            case Harmo.getBaseId harmo of
                Nothing ->
                    { m | gears = Coll.remove id gears }

                Just baseId ->
                    { m
                        | gears =
                            gears
                                |> Coll.update baseId (Harmo.remove id)
                                |> Coll.remove id
                    }


updateGear : Id Geer -> (Geer -> Geer) -> Mobeel -> Mobeel
updateGear =
    Content.updateGear


encoder : Mobeel -> E.Value
encoder =
    Content.mobileEncoder Wheel.encoder


decoder : D.Decoder Mobeel
decoder =
    Content.mobileDecoder Wheel.decoder Wheel.default
