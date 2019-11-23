module Link exposing (..)

import Coll exposing (Coll, Id)
import Color
import Fraction as Fract exposing (Fraction)
import Gear exposing (Gear, Ref)
import Math.Vector2 as Vec exposing (Vec2, vec2)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Length(..), Transform(..))


type alias Link =
    ( Id Gear, Id Gear )


viewFractLink : Coll Gear -> Link -> List (Svg msg)
viewFractLink gears l =
    let
        ( g, gg ) =
            toGears gears l
    in
    [ drawRawLink ( Gear.getPos g, Gear.getPos gg ) <|
        ((Gear.getLength g gears + Gear.getLength gg gears) / 2)
    ]


viewMotorLink : Coll Gear -> List Link -> Link -> List (Svg msg)
viewMotorLink gears cutting l =
    let
        ( g, gg ) =
            toGears gears l
    in
    [ S.g
        [ SA.opacity <|
            TypedSvg.Types.Opacity <|
                if List.any (equal l) cutting then
                    0.5

                else
                    1
        ]
        [ drawMotorLink
            ( ( Gear.getPos g, Gear.getLength g gears )
            , ( Gear.getPos gg, Gear.getLength gg gears )
            )
        ]
    ]


drawMotorLink : ( ( Vec2, Float ), ( Vec2, Float ) ) -> Svg msg
drawMotorLink ( ( p1, d1 ), ( p2, d2 ) ) =
    let
        dir =
            Vec.direction p2 p1

        contactPoint center diameter clockWise =
            Vec.add center <|
                Vec.scale (diameter / 2) (rotate90 dir clockWise)

        gearL =
            d1 + d2 / 2
    in
    S.g []
        [ drawRawLink ( contactPoint p1 d1 True, contactPoint p2 d2 True ) gearL
        , drawRawLink ( contactPoint p1 d1 False, contactPoint p2 d2 False ) gearL
        ]


drawRawLink : ( Vec2, Vec2 ) -> Float -> Svg msg
drawRawLink ( p1, p2 ) gearL =
    S.polyline
        [ SA.points [ tupleFromVec p1, tupleFromVec p2 ]
        , SA.stroke Color.brown
        , SA.strokeWidth <| Num (gearL / 30)
        , SA.strokeLinecap TypedSvg.Types.StrokeLinecapRound
        ]
        []


drawCut : ( Vec2, Vec2 ) -> Float -> Svg msg
drawCut ( p1, p2 ) scale =
    S.polyline
        [ SA.points [ tupleFromVec p1, tupleFromVec p2 ]
        , SA.strokeWidth <| Num (1 * scale)
        , SA.stroke Color.black
        , SA.shapeRendering <| TypedSvg.Types.RenderCrispEdges
        ]
        []


equal : Link -> Link -> Bool
equal l1 l2 =
    (Tuple.first l1 == Tuple.first l2 && Tuple.second l1 == Tuple.second l2)
        || (Tuple.first l1 == Tuple.second l2 && Tuple.first l2 == Tuple.second l1)


toGears : Coll Gear -> Link -> ( Gear, Gear )
toGears gears l =
    let
        getGear id =
            Coll.get id gears
    in
    Tuple.mapBoth getGear getGear l


toSegment : Coll Gear -> Link -> ( Vec2, Vec2 )
toSegment gears l =
    Tuple.mapBoth Gear.getPos Gear.getPos <| toGears gears l


tupleFromVec : Vec2 -> ( Float, Float )
tupleFromVec v =
    ( Vec.getX v, Vec.getY v )


rotate90 : Vec2 -> Bool -> Vec2
rotate90 v clockWise =
    if clockWise then
        vec2 (Vec.getY v) -(Vec.getX v)

    else
        vec2 -(Vec.getY v) (Vec.getX v)



-- from https://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect


cuts : ( Vec2, Vec2 ) -> ( Vec2, Vec2 ) -> Bool
cuts ( p, p2 ) ( q, q2 ) =
    let
        r =
            Vec.sub p2 p

        s =
            Vec.sub q2 q

        rs =
            crossProductLength r s
    in
    if rs == 0 then
        False

    else
        let
            qp =
                Vec.sub q p

            t =
                crossProductLength qp s / rs

            u =
                crossProductLength qp r / rs
        in
        if (t > 0) && (t < 1) && (u > 0) && (u < 1) then
            True

        else
            False


crossProductLength : Vec2 -> Vec2 -> Float
crossProductLength v w =
    Vec.getX v * Vec.getY w - Vec.getY v * Vec.getX w
