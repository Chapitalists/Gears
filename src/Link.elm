module Link exposing (..)

import Color exposing (Color)
import Json.Decode as D
import Json.Decode.Field as Field
import Json.Encode as E
import Math.Vector2 as Vec exposing (Vec2, vec2)
import Tools.Coll as Coll exposing (Coll, Id)
import Tools.Fraction exposing (Fraction)
import Tools.Interact as Interact
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as Svg exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), DominantBaseline(..), Length(..), Opacity(..), Transform(..))


baseColor : Color
baseColor =
    Color.brown


type alias Link item =
    ( Id item, Id item )


map : Link a -> Link b
map ( i, j ) =
    ( Coll.idMap i, Coll.idMap j )


type alias Circle =
    { d : Float, c : Vec2 }


type alias Segment =
    ( Vec2, Vec2 )


type alias DrawLink =
    ( Circle, Circle )


viewFractLink : DrawLink -> inter -> List (Svg (Interact.Msg inter zone))
viewFractLink ( e, f ) inter =
    [ drawRawLink ( e.c, f.c ) ((e.d + f.d) / 2) baseColor
    , S.polyline
        (Interact.hoverEvents inter
            ++ [ SA.points [ tupleFromVec e.c, tupleFromVec f.c ]
               , SA.strokeWidth <| Num ((e.d + f.d) / 15)
               , SA.strokeOpacity <| Opacity 0
               , SA.stroke Color.black
               ]
        )
        []
    ]


viewFractOnLink : DrawLink -> Fraction -> List (Svg msg)
viewFractOnLink ( e, f ) { num, den } =
    let
        dir =
            Vec.normalize <| Vec.sub f.c e.c

        center =
            Vec.scale 0.5 <| Vec.add (Vec.add e.c <| Vec.scale (e.d / 2) dir) (Vec.sub f.c <| Vec.scale (f.d / 2) dir)

        d =
            Vec.normalize <|
                rotate90 (Vec.sub e.c f.c) <|
                    not <|
                        Vec.getX e.c
                            < Vec.getX f.c
                            || (Vec.getX e.c == Vec.getX f.c && Vec.getY e.c < Vec.getY f.c)

        size =
            (e.d + f.d) / 10

        txt mult i =
            let
                p =
                    Vec.add center <| Vec.scale (mult * size / 2) d
            in
            S.text_
                [ SA.x <| Num <| Vec.getX p
                , SA.y <| Num <| Vec.getY p
                , SA.fontSize <| Num size
                , SA.textAnchor AnchorMiddle
                , SA.dominantBaseline DominantBaselineCentral
                , SA.stroke Color.white
                , SA.strokeWidth <| Num (size / 40)
                ]
                [ Svg.text <| String.fromInt i ]
    in
    [ txt 1 num, txt -1 den ]


viewSelectedLink : DrawLink -> Maybe Fraction -> List (Svg msg)
viewSelectedLink ( e, f ) mayFract =
    drawRawLink ( e.c, f.c ) ((e.d + f.d) / 2) Color.red
        :: (Maybe.withDefault [] <| Maybe.map (viewFractOnLink ( e, f )) mayFract)


viewMotorLink : Bool -> DrawLink -> List (Svg msg)
viewMotorLink cutting dl =
    [ S.g
        [ SA.opacity <|
            TypedSvg.Types.Opacity <|
                if cutting then
                    0.5

                else
                    1
        ]
        [ drawMotorLink dl ]
    ]


drawMotorLink : DrawLink -> Svg msg
drawMotorLink ( c1, c2 ) =
    let
        p1 =
            c1.c

        d1 =
            c1.d

        p2 =
            c2.c

        d2 =
            c2.d

        gearL =
            d1 + d2 / 2

        ( a1, b1 ) =
            getPerpandicularDiameter c1 ( p1, p2 )

        ( a2, b2 ) =
            getPerpandicularDiameter c2 ( p1, p2 )
    in
    S.g []
        [ drawRawLink ( a1, a2 ) gearL baseColor
        , drawRawLink ( b1, b2 ) gearL baseColor
        ]


drawRawLink : Segment -> Float -> Color -> Svg msg
drawRawLink ( p1, p2 ) gearL c =
    S.polyline
        [ SA.points [ tupleFromVec p1, tupleFromVec p2 ]
        , SA.stroke c
        , SA.strokeWidth <| Num (gearL / 30)
        , SA.strokeLinecap TypedSvg.Types.StrokeLinecapRound
        ]
        []


drawCut : Segment -> Float -> Svg msg
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


toSegment : DrawLink -> Segment
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


getPerpandicularDiameter : Circle -> Segment -> Segment
getPerpandicularDiameter { c, d } ( p1, p2 ) =
    let
        dir =
            Vec.direction p2 p1

        projection center diameter clockWise =
            Vec.add center <|
                Vec.scale (diameter / 2) (rotate90 dir clockWise)
    in
    ( projection c d True, projection c d False )



-- from https://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect


cuts : Segment -> Segment -> Bool
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


touchCircle : Segment -> Circle -> Bool
touchCircle segment circle =
    cuts segment <| getPerpandicularDiameter circle segment


crossProductLength : Vec2 -> Vec2 -> Float
crossProductLength v w =
    Vec.getX v * Vec.getY w - Vec.getY v * Vec.getX w
