module Link exposing (..)

import Coll exposing (Coll, Id)
import Color
import Fraction as Fract exposing (Fraction)
import Json.Decode as D
import Json.Decode.Field as Field
import Json.Encode as E
import Math.Vector2 as Vec exposing (Vec2, vec2)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Length(..), Transform(..))


type alias Link item =
    ( Id item, Id item )


map : Link a -> Link b
map ( i, j ) =
    ( Coll.idMap i, Coll.idMap j )


type alias Circle =
    { d : Float, c : Vec2 }


type alias DrawLink =
    ( Circle, Circle )


viewFractLink : DrawLink -> List (Svg msg)
viewFractLink ( e, f ) =
    [ drawRawLink ( e.c, f.c ) <|
        ((e.d + f.d) / 2)
    ]


viewMotorLink : Bool -> DrawLink -> List (Svg msg)
viewMotorLink cutting ( e, f ) =
    [ S.g
        [ SA.opacity <|
            TypedSvg.Types.Opacity <|
                if cutting then
                    0.5

                else
                    1
        ]
        [ drawMotorLink
            ( ( e.c, e.d )
            , ( f.c, f.d )
            )
        ]
    ]


viewSelectedLink : DrawLink -> List (Svg msg)
viewSelectedLink ( e, f ) =
    let
        w =
            (e.d + f.d) / 30
    in
    [ S.polyline
        [ SA.points [ tupleFromVec <| e.c, tupleFromVec <| f.c ]
        , SA.stroke Color.red
        , SA.strokeWidth <| Num w
        , SA.strokeLinecap TypedSvg.Types.StrokeLinecapRound
        ]
        []
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


encoder : Link x -> E.Value
encoder l =
    E.object
        [ ( "from", Coll.idEncoder <| Tuple.first l )
        , ( "to", Coll.idEncoder <| Tuple.second l )
        ]


decoder : D.Decoder (Link x)
decoder =
    Field.require "from" Coll.idDecoder <|
        \from ->
            Field.require "to" Coll.idDecoder <|
                \to ->
                    D.succeed ( from, to )


equal : Link x -> Link x -> Bool
equal l1 l2 =
    (Tuple.first l1 == Tuple.first l2 && Tuple.second l1 == Tuple.second l2)
        || (Tuple.first l1 == Tuple.second l2 && Tuple.first l2 == Tuple.second l1)


toSegment : DrawLink -> ( Vec2, Vec2 )
toSegment l =
    Tuple.mapBoth .c .c l


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
