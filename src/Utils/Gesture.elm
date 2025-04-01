module Utils.Gesture exposing (..)

import Color exposing (Color)
import Data.Wheel as Wheel exposing (Wheel)
import Math.Vector2 as Vec2 exposing (Vec2)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as Svg exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Length(..), Opacity(..), Transform(..))
import Utils.PanSvg as PanSvg exposing (PanSvg)
import Utils.Utils exposing (Size)


dragZoneAngle : Float
dragZoneAngle =
    pi / 8


dragZoneColor : Color
dragZoneColor =
    Color.lightBlue


view : PanSvg -> Size -> Wheel -> Svg msg
view svg { width, height } w =
    let
        vecP =
            Wheel.getPos w

        scale =
            toFloat (max width height) * PanSvg.getScale svg

        x =
            scale * cos dragZoneAngle

        y =
            scale * sin dragZoneAngle

        zone p1 p2 =
            S.polygon
                [ SA.transform
                    [ Translate (Vec2.getX vecP) (Vec2.getY vecP) ]
                , SA.fill <| Fill dragZoneColor
                , SA.strokeWidth <| Num 0
                , SA.points [ ( 0, 0 ), p1, p2 ]
                ]
                []
    in
    S.g [ SA.opacity <| Opacity 0.5 ]
        [ zone ( x, y ) ( x, -y )
        , zone ( -x, y ) ( -x, -y )
        , zone ( y, x ) ( -y, x )
        , zone ( y, -x ) ( -y, -x )
        ]
