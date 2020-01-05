module Collar exposing (..)

import Content exposing (Bead, Collar, Content)
import Editor.Common exposing (..)
import Json.Decode as D
import Json.Encode as E
import Wheel exposing (Wheel)


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


fromWheel : Wheel -> Float -> Colleer
fromWheel w l =
    { matrice = l
    , loop = l
    , head = { length = l, wheel = w }
    , beads = []
    }


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


get : Int -> Colleer -> Beed
get i c =
    Maybe.withDefault
        (Debug.log ("Cannot get Bead " ++ String.fromInt i) <| c.head)
    <|
        List.head <|
            List.drop i <|
                Content.getBeads c


add : Int -> Beed -> Colleer -> Colleer
add i b c =
    if i <= 0 then
        { c | head = b, beads = c.head :: c.beads }

    else
        { c | beads = List.concat [ List.take (i - 1) c.beads, [ b ], List.drop (i - 1) c.beads ] }


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
