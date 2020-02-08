module Data.Collar exposing (..)

import Data.Common exposing (..)
import Data.Content as Content exposing (Bead, Collar, Content)
import Data.Wheel as Wheel exposing (Conteet, Wheel)
import Json.Decode as D
import Json.Encode as E


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
getCumulLengthAt =
    Content.getCumulLengthAt


get : Int -> Colleer -> Beed
get =
    Content.getBead


add : Int -> Beed -> Colleer -> Colleer
add i b c =
    if i <= 0 then
        { c
            | head = b
            , beads = c.head :: c.beads
            , matrice = c.matrice + 1
        }

    else
        { c
            | beads = List.concat [ List.take (i - 1) c.beads, [ b ], List.drop (i - 1) c.beads ]
            , matrice = c.matrice + 1
        }


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
                }

            ( j, beads ) ->
                { c
                    | beads = List.concat [ List.take (j - 1) beads, List.drop j beads ]
                    , matrice =
                        if c.matrice > j then
                            c.matrice - 1

                        else
                            c.matrice
                }


updateBead : Int -> (Beed -> Beed) -> Colleer -> Colleer
updateBead =
    Content.updateBead


encoder : Colleer -> E.Value
encoder =
    Content.collarEncoder Wheel.encoder


decoder : D.Decoder Colleer
decoder =
    Content.collarDecoder Wheel.decoder
