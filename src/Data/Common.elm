module Data.Common exposing (..)

import Data.Content as Content exposing (Content)
import Data.Wheel exposing (Wheel)
import Harmony as Harmo
import Sound


getContentLength : Content Wheel -> Float
getContentLength c =
    case c of
        Content.S s ->
            Sound.length s

        Content.M m ->
            Harmo.getLengthId m.motor m.gears

        Content.C col ->
            Content.getCumulLengthAt col.matrice col
