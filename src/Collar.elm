module Collar exposing (..)

import Content exposing (Bead, Collar)
import Json.Decode as D
import Json.Encode as E
import TypedSvg.Core exposing (Svg)
import Wheel exposing (Wheel)


type alias Colleer =
    Collar Wheel


type alias Beed =
    Bead Wheel


view : Colleer -> List (Svg msg)
view c =
    []


encoder : Colleer -> E.Value
encoder =
    Content.collarEncoder Wheel.encoder


decoder : D.Decoder Colleer
decoder =
    Content.collarDecoder Wheel.decoder
