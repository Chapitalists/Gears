module Data.Common exposing (..)

import Coll exposing (Id)
import Data.Content as Content exposing (Bead, Collar, Content, Mobile)
import Data.Gear as Gear exposing (Gear)
import Data.Wheel as Wheel exposing (Conteet, Wheel, Wheeled)
import Harmony as Harmo
import Sound



-- TODO Create Geer Mobeel Colleer here


type alias Identifier =
    ( Id (Gear Wheel), List Int )


getName : Identifier -> Mobile Wheel -> String
getName ( id, l ) mobile =
    let
        name =
            (getWheel ( id, l ) mobile).name
    in
    if String.isEmpty name then
        case l of
            [] ->
                Gear.toUID id

            _ ->
                "beadTODO"

    else
        name


getWheel : Identifier -> Mobile Wheel -> Wheel
getWheel ( id, list ) m =
    let
        f : List Int -> Wheel -> Wheel
        f l w =
            case l of
                [] ->
                    w

                i :: ll ->
                    case Wheel.getWheelContent w of
                        Content.C col ->
                            f ll (Content.getBead i col).wheel

                        _ ->
                            Debug.log ("Wrong identifier to get " ++ (String.concat <| List.map String.fromInt l)) w
    in
    f list (Content.getGear id m).wheel


deleteWheel :
    Identifier
    -> Mobile Wheel
    -> (Id (Gear Wheel) -> Mobile Wheel -> Mobile Wheel)
    -> (Int -> Collar Wheel -> Collar Wheel)
    -> Mobile Wheel
deleteWheel ( id, l ) mobile gRm bRm =
    let
        rec : Int -> List Int -> Collar Wheel -> Collar Wheel
        rec index list col =
            case list of
                [] ->
                    bRm index col

                i :: rest ->
                    case Wheel.getContent <| Content.getBead index col of
                        Content.C subCol ->
                            Content.updateBead index (Wheel.setContent <| Content.C <| rec i rest subCol) col

                        _ ->
                            Debug.log "Wrong identifier to delete bead" col
    in
    case l of
        [] ->
            gRm id mobile

        [ i ] ->
            case Wheel.getContent <| Coll.get id mobile.gears of
                Content.C col ->
                    Content.updateGear id (Wheel.setContent <| Content.C <| bRm i col) mobile

                _ ->
                    Debug.log "Wrong identifier to delete bead" mobile

        i :: rest ->
            case Wheel.getContent <| Coll.get id mobile.gears of
                Content.C col ->
                    Content.updateGear id (Wheel.setContent <| Content.C <| rec i rest col) mobile

                _ ->
                    Debug.log "Wrong identifier to delete bead" mobile


updateWheel : Identifier -> Wheel.Msg -> Mobile Wheel -> Mobile Wheel
updateWheel ( id, list ) msg m =
    let
        rec : List Int -> Wheel -> Wheel
        rec l w =
            case l of
                [] ->
                    -- TODO Absurd to go through Wheeled if managing Wheel? Change or add to Wheel API, or refactor
                    (Wheel.update msg { wheel = w }).wheel

                i :: ll ->
                    case Wheel.getWheelContent w of
                        Content.C col ->
                            (Wheel.setContent
                                (Content.C <| Content.updateBead i (\bead -> { bead | wheel = rec ll bead.wheel }) col)
                                { wheel = w }
                            ).wheel

                        _ ->
                            Debug.log ("Wrong identifier to update " ++ (String.concat <| List.map String.fromInt l)) w
    in
    Content.updateGear id (\gear -> { gear | wheel = rec list gear.wheel }) m


getContentLength : Conteet -> Float
getContentLength c =
    case c of
        Content.S s ->
            Sound.length s

        Content.M m ->
            Harmo.getLengthId m.motor m.gears

        Content.C col ->
            Content.getMatriceLength col
