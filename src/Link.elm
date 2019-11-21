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
        getGear id =
            Coll.get id gears
    in
    case Tuple.mapBoth getGear getGear l of
        ( Just g, Just gg ) ->
            [ drawRawLink ( Gear.getPos g, Gear.getPos gg ) <|
                (Gear.getLength g gears + Gear.getLength gg gears)
                    / 2
            ]

        _ ->
            Debug.log "Didn’t found both gears to draw algebraic link" []


viewMotorLink : Coll Gear -> Link -> List (Svg msg)
viewMotorLink gears l =
    let
        getGear id =
            Coll.get id gears
    in
    case Tuple.mapBoth getGear getGear l of
        ( Just g, Just gg ) ->
            [ drawMotorLink
                ( ( Gear.getPos g, Gear.getLength g gears )
                , ( Gear.getPos gg, Gear.getLength gg gears )
                )
            ]

        _ ->
            Debug.log "Didn’t found both gears to draw motor link" []


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
        , SA.stroke <| Color.brown
        , SA.strokeWidth <| Num (gearL / 30)
        , SA.strokeLinecap TypedSvg.Types.StrokeLinecapRound
        ]
        []


tupleFromVec : Vec2 -> ( Float, Float )
tupleFromVec v =
    ( Vec.getX v, Vec.getY v )


rotate90 : Vec2 -> Bool -> Vec2
rotate90 v clockWise =
    if clockWise then
        vec2 (Vec.getY v) -(Vec.getX v)

    else
        vec2 -(Vec.getY v) (Vec.getX v)
