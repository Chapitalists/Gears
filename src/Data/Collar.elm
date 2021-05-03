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
    { matrice = 1
    , loop = 0
    , head = { length = l, wheel = w }
    , beads = []
    , oneSound = Nothing
    }


fromBeads : Beed -> List Beed -> Colleer
fromBeads head rest =
    { matrice = List.length rest + 1
    , loop = 0
    , head = head
    , beads = rest
    , oneSound = Nothing
    }


fromWheelMult : Wheel -> Int -> Float -> Colleer
fromWheelMult w m l =
    { matrice = m
    , loop = 0
    , head = { length = l, wheel = w }
    , beads = List.repeat (m - 1) { length = l, wheel = w }
    , oneSound = Nothing
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
            { matrice = d
            , loop = 0
            , head = head
            , beads = rest
            , oneSound =
                Just
                    { path = Sound.getPath s
                    , start = Tuple.first loopPercents
                    , end = Tuple.second loopPercents
                    , divs = divs
                    }
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
    List.foldl (\b sum -> sum + b.length) 0 << getBeads


getMinLength : Colleer -> Float
getMinLength =
    List.foldl (\b m -> min m b.length) (1 / 0) << getBeads


getMaxLength : Colleer -> Float
getMaxLength =
    List.foldl (\b m -> max m b.length) 0 << getBeads


getCumulLengthAt : Int -> Colleer -> Float
getCumulLengthAt =
    Content.getCumulLengthAt


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
                    , matrice = c.matrice + List.length bs
                    , oneSound = Nothing
                }

            else
                { c
                    | beads = List.concat [ List.take (i - 1) c.beads, bs, List.drop (i - 1) c.beads ]
                    , matrice = c.matrice + List.length bs
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
                    , matrice = c.matrice - 1
                    , oneSound = Nothing
                }

            ( j, beads ) ->
                { c
                    | beads = List.concat [ List.take (j - 1) beads, List.drop j beads ]
                    , matrice =
                        if c.matrice > j then
                            c.matrice - 1

                        else
                            c.matrice
                    , oneSound = Nothing
                }


updateBead : Int -> (Beed -> Beed) -> Colleer -> Colleer
updateBead =
    Content.updateBead


encoder : Colleer -> E.Value
encoder =
    Content.collarEncoder Wheel.encoder


decoder : D.Decoder Colleer
decoder =
    Content.collarDecoder (Wheel.decoder getContentLength)
