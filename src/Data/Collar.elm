module Data.Collar exposing (..)

import Data.Common exposing (..)
import Data.Content as Content exposing (Bead, Collar, Content)
import Data.Wheel as Wheel exposing (Wheel)
import Json.Decode as D
import Json.Encode as E


type alias Colleer =
    Collar Wheel


type alias Beed =
    Bead Wheel


toUID : Int -> String
toUID i =
    "bead-" ++ String.fromInt i


beadFromContent : Content Wheel -> Beed
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
    { matrice = l
    , loop = l
    , head = { length = l, wheel = w }
    , beads = []
    }


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
getCumulLengthAt i c =
    List.foldl (\b sum -> sum + b.length) 0 <| List.take i <| getBeads c


get : Int -> Colleer -> Beed
get i c =
    case List.head <| List.drop i <| getBeads c of
        Just b ->
            b

        Nothing ->
            Debug.log ("Cannot get Bead " ++ String.fromInt i) <| c.head


add : Int -> Beed -> Colleer -> Colleer
add i b c =
    if i <= 0 then
        { c | head = b, beads = c.head :: c.beads }

    else
        { c | beads = List.concat [ List.take (i - 1) c.beads, [ b ], List.drop (i - 1) c.beads ] }


rm : Int -> Colleer -> Colleer
rm i c =
    if i < 0 || i > List.length c.beads then
        c

    else
        case ( i, c.beads ) of
            ( 0, head :: beads ) ->
                { c | head = head, beads = beads }

            ( j, beads ) ->
                { c | beads = List.concat [ List.take (j - 1) beads, List.drop j beads ] }


updateBead : Int -> (Beed -> Beed) -> Colleer -> Colleer
updateBead i f c =
    if i <= 0 then
        { c | head = f c.head }

    else
        { c
            | beads =
                List.concat
                    [ List.take (i - 1) c.beads
                    , case List.head <| List.drop (i - 1) c.beads of
                        Nothing ->
                            []

                        Just b ->
                            [ f b ]
                    , List.drop i c.beads
                    ]
        }


encoder : Colleer -> E.Value
encoder =
    Content.collarEncoder Wheel.encoder


decoder : D.Decoder Colleer
decoder =
    Content.collarDecoder Wheel.decoder
