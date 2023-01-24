module Data.Collar exposing (..)

import Data.Common exposing (..)
import Data.Content as Content exposing (Bead, Collar, Content)
import Data.Wheel as Wheel exposing (Conteet, Wheel)
import Json.Decode as D
import Json.Encode as E
import Sound exposing (Sound)


type alias Colleer =
    Collar Wheel


type alias Beed =
    Bead Wheel


toUID : Int -> String
toUID i =
    "bead-" ++ String.fromInt i


beadFromContent : Conteet -> Beed
beadFromContent c =
    { length = getContentLength c, wheel = Wheel.fromContent c }


beadName : Int -> Colleer -> String
beadName i collar =
    let
        name =
            (get i collar).wheel.name
    in
    if String.isEmpty name then
        toUID i

    else
        name


fromWheel : Wheel -> Float -> Colleer
fromWheel w l =
    { head = { length = l, wheel = w }
    , beads = []
    , oneSound = Nothing
    , mvShift = False
    }


fromBeads : Beed -> List Beed -> Colleer
fromBeads head rest =
    { head = head
    , beads = rest
    , oneSound = Nothing
    , mvShift = False
    }


fromWheelMult : Wheel -> Int -> Float -> Colleer
fromWheelMult w m l =
    { head = { length = l, wheel = w }
    , beads = List.repeat (m - 1) { length = l, wheel = w }
    , oneSound = Nothing
    , mvShift = False
    }


fromSoundDiv : Sound -> Int -> Float -> Colleer
fromSoundDiv s d l =
    let
        ( sounds, divs ) =
            Sound.divide d s

        loopPercents =
            Sound.getLoopPercents s

        contents =
            List.map Content.S sounds

        beads =
            List.map (\c -> { length = l / toFloat d, wheel = Wheel.fromContent c }) contents
    in
    case beads of
        head :: rest ->
            { head = head
            , beads = rest
            , oneSound =
                Just
                    { path = Sound.getPath s
                    , start = Tuple.first loopPercents
                    , end = Tuple.second loopPercents
                    , divs = divs
                    }
            , mvShift = False
            }

        _ ->
            fromWheel (Wheel.fromContent <| Content.S s) l


length : Colleer -> Int
length =
    List.length << getBeads


getBeads : Colleer -> List Beed
getBeads =
    Content.getBeads


getTotalLength : Colleer -> Float
getTotalLength =
    Content.getCollarLength


getCumulLengthAt : Int -> Colleer -> Float
getCumulLengthAt i =
    List.foldl (\b sum -> sum + b.length) 0 << List.take i << getBeads


getMinLength : Colleer -> Float
getMinLength =
    List.foldl (\b m -> min m b.length) (1 / 0) << getBeads


getMaxLength : Colleer -> Float
getMaxLength =
    List.foldl (\b m -> max m b.length) 0 << getBeads


get : Int -> Colleer -> Beed
get =
    Content.getBead


addBeads : Int -> List Beed -> Colleer -> Colleer
addBeads i bs c =
    case bs of
        [] ->
            c

        head :: tail ->
            if i <= 0 then
                { c
                    | head = head
                    , beads = List.concat [ tail, c.head :: c.beads ]
                    , oneSound = Nothing
                }

            else
                { c
                    | beads = List.concat [ List.take (i - 1) c.beads, bs, List.drop (i - 1) c.beads ]
                    , oneSound = Nothing
                }


add : Int -> Beed -> Colleer -> Colleer
add i b =
    addBeads i [ b ]


rm : Int -> Colleer -> Colleer
rm i c =
    if i < 0 || i > List.length c.beads || List.length c.beads == 0 then
        c

    else
        case ( i, c.beads ) of
            ( 0, head :: beads ) ->
                { c
                    | head = head
                    , beads = beads
                    , oneSound = Nothing
                }

            ( j, beads ) ->
                { c
                    | beads = List.concat [ List.take (j - 1) beads, List.drop j beads ]
                    , oneSound = Nothing
                }


mv : Int -> Float -> Colleer -> Colleer
mv index lengthD c =
    let
        i =
            if c.mvShift then
                index + 1

            else
                index

        oldBeads =
            getBeads c
    in
    case List.head <| List.drop i oldBeads of
        Nothing ->
            c

        Just movedBead ->
            let
                preBeads =
                    List.take (i - 1) oldBeads

                mayBeadBefore =
                    if i == 0 then
                        Nothing

                    else
                        List.head <| List.drop (i - 1) oldBeads

                postBeads =
                    List.drop (i + 2) oldBeads

                mayBeadAfter =
                    List.head <| List.drop (i + 1) oldBeads

                return shift l =
                    case l of
                        head :: beads ->
                            { c
                                | head = head
                                , beads = beads
                                , oneSound = Nothing
                                , mvShift = shift
                            }

                        [] ->
                            c
            in
            if lengthD < 0 then
                -- MOVE LEFT
                case mayBeadBefore of
                    Nothing ->
                        c

                    Just beadBefore ->
                        let
                            d =
                                clamp 0 beadBefore.length -lengthD

                            beadAfter =
                                case mayBeadAfter of
                                    Just b ->
                                        { b | length = b.length + d }

                                    Nothing ->
                                        { length = d, wheel = Wheel.default }
                        in
                        return c.mvShift <|
                            preBeads
                                ++ [ { beadBefore | length = beadBefore.length - d } ]
                                ++ [ movedBead ]
                                ++ [ beadAfter ]
                                ++ postBeads

            else
                -- MOVE RIGHT
                case mayBeadAfter of
                    Nothing ->
                        c

                    Just beadAfter ->
                        let
                            d =
                                clamp 0 beadAfter.length lengthD

                            ( beadBefore, shift ) =
                                case mayBeadBefore of
                                    Just b ->
                                        ( { b | length = b.length + d }
                                        , c.mvShift
                                        )

                                    Nothing ->
                                        ( { length = d, wheel = Wheel.default }
                                        , True
                                        )
                        in
                        return shift <|
                            preBeads
                                ++ [ beadBefore ]
                                ++ [ movedBead ]
                                ++ [ { beadAfter | length = beadAfter.length - d } ]
                                ++ postBeads


clean : Colleer -> Colleer
clean c =
    let
        beads =
            List.filter (\b -> b.length > 0) c.beads
    in
    if c.head.length > 0 then
        { c | beads = beads, mvShift = False }

    else
        case beads of
            head :: tail ->
                { c | head = head, beads = tail, mvShift = False }

            [] ->
                c


updateBead : Int -> (Beed -> Beed) -> Colleer -> Colleer
updateBead =
    Content.updateBead


encoder : Colleer -> E.Value
encoder =
    Content.collarEncoder Wheel.encoder


decoder : D.Decoder Colleer
decoder =
    Content.collarDecoder (Wheel.decoder getContentLength)
