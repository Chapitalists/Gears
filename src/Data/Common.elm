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
getName id mobile =
    let
        w =
            getWheel id mobile
    in
    if String.isEmpty w.name then
        case Wheel.getWheelContent w of
            Content.S s ->
                Sound.fileNameFromPath <| Sound.getPath s

            Content.C c ->
                case c.oneSound of
                    Just { path } ->
                        Sound.fileNameFromPath path

                    Nothing ->
                        toUid id

            _ ->
                toUid id

    else
        w.name


toUid : Identifier -> String
toUid ( id, l ) =
    List.foldl (\i uid -> Content.beadUIDExtension uid i) (Gear.toUID id) l


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
                            let
                                _ =
                                    Debug.log ("Wrong identifier to get " ++ (String.concat <| List.map String.fromInt l)) ( id, list, m )
                            in
                            w
    in
    f list (Content.getGear id m).wheel


deleteWheel :
    Identifier
    -> Mobile Wheel
    -> (Id (Gear Wheel) -> Mobile Wheel -> Mobile Wheel)
    -> (Int -> Collar Wheel -> Collar Wheel)
    -> ( Mobile Wheel, Bool ) -- True if there is only one bead left after deleting
deleteWheel ( id, l ) mobile gRm bRm =
    let
        rec : Int -> List Int -> Collar Wheel -> ( Collar Wheel, Bool )
        rec index list col =
            case list of
                [] ->
                    let
                        newCol =
                            bRm index col
                    in
                    ( newCol, (List.length <| Content.getBeads newCol) == 1 )

                i :: rest ->
                    case Wheel.getContent <| Content.getBead index col of
                        Content.C subCol ->
                            let
                                ( newSubCol, last ) =
                                    rec i rest subCol
                            in
                            ( Content.updateBead index (Wheel.setContent <| Content.C newSubCol) col, last )

                        _ ->
                            let
                                _ =
                                    Debug.log "Wrong identifier to delete bead" ( id, l, mobile )
                            in
                            ( col, False )
    in
    case l of
        [] ->
            ( gRm id mobile, False )

        i :: rest ->
            case Wheel.getContent <| Coll.get id mobile.gears of
                Content.C col ->
                    let
                        ( newCol, last ) =
                            rec i rest col
                    in
                    ( Content.updateGear id (Wheel.setContent <| Content.C newCol) mobile, last )

                _ ->
                    let
                        _ =
                            Debug.log "Wrong identifier to delete bead" ( id, l, mobile )
                    in
                    ( mobile, False )


updateWheel : Identifier -> Wheel.Msg -> Mobile Wheel -> Mobile Wheel
updateWheel ( id, list ) msg m =
    let
        ( modify, upBead ) =
            case msg of
                Wheel.ChangeContent _ ->
                    ( True, Content.updateBead )

                Wheel.ChangeStart _ ->
                    ( True, Content.updateBead )

                Wheel.ChangeLoop _ ->
                    ( True, Content.updateBead )

                _ ->
                    ( False, Content.updateBeadKeepOneSound )

        rec : List Int -> Wheel -> Wheel
        rec l w =
            case l of
                [] ->
                    -- TODO Absurd to go through Wheeled if managing Wheel? Change or add to Wheel API, or refactor
                    (Wheel.update msg { wheel = w }).wheel

                i :: ll ->
                    case Wheel.getWheelContent w of
                        Content.C col ->
                            let
                                upCol =
                                    upBead i (\bead -> { bead | wheel = rec ll bead.wheel }) col

                                newCol =
                                    if modify then
                                        { upCol | oneSound = Nothing }

                                    else
                                        upCol
                            in
                            (Wheel.setContent (Content.C newCol) { wheel = w }).wheel

                        _ ->
                            let
                                _ =
                                    Debug.log "Wrong identifier to update bead" ( ( id, l ), msg, m )
                            in
                            w
    in
    Content.updateGear id (\gear -> { gear | wheel = rec list gear.wheel }) m


getWheeledContentLength : Wheeled g -> Float
getWheeledContentLength =
    getContentLength << Wheel.getContent


getContentLength : Conteet -> Float
getContentLength c =
    case c of
        Content.S s ->
            Sound.length s

        Content.M m ->
            Harmo.getLengthId getWheeledContentLength m.motor m.gears

        Content.C col ->
            Content.getMatriceLength col
