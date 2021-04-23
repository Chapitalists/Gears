module Data.Mobile exposing (..)

import Coll exposing (Coll, Id)
import Data.Common exposing (..)
import Data.Content as Content exposing (Content, Mobile)
import Data.Gear as Gear exposing (Gear)
import Data.Wheel as Wheel exposing (Conteet, Wheel)
import Harmony as Harmo exposing (Harmony)
import Json.Decode as D
import Json.Encode as E
import Link exposing (DrawLink, Link)
import Math.Vector2 as Vec exposing (Vec2)
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
    , harmony = Harmo.newRate 1
    , motor = []
    , wheel = Wheel.fromContent c
    }


newSizedGear : Vec2 -> Float -> Wheel -> Geer
newSizedGear p l w =
    { pos = p, harmony = Harmo.newRate (l / getWheeledContentLength { wheel = w }), motor = [], wheel = w }


copy : Bool -> Vec2 -> Id Geer -> Coll Geer -> Coll Geer
copy harmo move id coll =
    let
        g =
            Coll.get id coll

        newG =
            { g
                | pos = Vec.add g.pos move
                , motor = []
            }

        ( newId, newColl ) =
            Coll.insertTellId newG coll
    in
    if harmo then
        Harmo.makeCopy id newId newColl

    else
        Harmo.toRate getWheeledContentLength newId newColl


toDrawLink : Coll Geer -> Link Geer -> DrawLink
toDrawLink coll l =
    let
        get id =
            Coll.get id coll

        toCircle g =
            { c = g.pos, d = getLength g coll }

        f =
            get >> toCircle
    in
    Tuple.mapBoth f f l


getLengthId : Id Geer -> Coll Geer -> Float
getLengthId =
    Harmo.getLengthId getWheeledContentLength


getLength : Geer -> Coll Geer -> Float
getLength =
    Harmo.getLength getWheeledContentLength


gearPosSize : Id Geer -> Coll Geer -> ( Vec2, Float )
gearPosSize id coll =
    let
        g =
            Coll.get id coll
    in
    ( g.pos, getLength g coll )


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
            let
                _ =
                    Debug.log "TODO delete base" []
            in
            m

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
    Content.mobileDecoder (Wheel.decoder getContentLength) (getContentLength << Wheel.getWheelContent) Wheel.default
