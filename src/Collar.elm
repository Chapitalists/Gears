module Collar exposing (..)

import Coll
import Content exposing (Bead, Collar)
import Interact
import Json.Decode as D
import Json.Encode as E
import Math.Vector2 exposing (vec2)
import Sound exposing (Sound)
import TypedSvg.Core exposing (Svg)
import Wheel exposing (Wheel)


type alias Colleer =
    Collar Wheel


type alias Beed =
    Bead Wheel


toUID : Int -> String
toUID i =
    "bead-" ++ String.fromInt i


beadFromSound : Sound -> Beed
beadFromSound s =
    { length = Sound.length s, wheel = Wheel.fromContent <| Content.S s }


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


view : Colleer -> List (Svg (Interact.Msg (Wheel.Interactable item)))
view c =
    List.foldl
        (\b ( l, ( p, i ) ) ->
            ( Wheel.view b.wheel
                (vec2 (p + b.length / 2) 50)
                b.length
                { mod = Wheel.None, motor = False, dashed = False }
                Coll.startId
                (toUID i)
                :: l
            , ( p + b.length
              , i + 1
              )
            )
        )
        ( [], ( 50, 0 ) )
        (getBeads c)
        |> Tuple.first


encoder : Colleer -> E.Value
encoder =
    Content.collarEncoder Wheel.encoder


decoder : D.Decoder Colleer
decoder =
    Content.collarDecoder Wheel.decoder
