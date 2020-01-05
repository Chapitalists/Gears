module Editor.Common exposing (..)

import Content exposing (Content)
import Harmony as Harmo
import Sound
import Wheel exposing (Wheel)


getContentLength : Content Wheel -> Float
getContentLength c =
    case c of
        Content.S s ->
            Sound.length s

        Content.M m ->
            Harmo.getLengthId m.motor m.gears

        Content.C col ->
            col.matrice
