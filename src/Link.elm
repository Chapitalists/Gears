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


view : Coll Gear -> Coll Ref -> Link -> List (Svg msg)
view gears refs l =
    let
        getGear id =
            Coll.get id gears

        getRef id =
            Coll.get id refs
    in
    case Tuple.mapBoth getGear getGear l of
        ( Just g, Just gg ) ->
            let
                r =
                    Gear.getRefId g

                rr =
                    Gear.getRefId gg
            in
            if r == rr then
                case getRef r of
                    Just ref ->
                        drawRefLink ( g, gg, ref )

                    Nothing ->
                        Debug.log "Not found common ref to draw link" []

            else
                case ( getRef r, getRef rr ) of
                    ( Just ref, Just rref ) ->
                        [ drawLink ( Gear.getPos g, Gear.getPos gg ) <|
                            min (Gear.getLength ( g, ref )) (Gear.getLength ( gg, rref ))
                        ]

                    _ ->
                        Debug.log "Miss one ref to draw link" []

        _ ->
            Debug.log "Didn’t found both gears to draw link" []


drawLink : ( Vec2, Vec2 ) -> Float -> Svg msg
drawLink ( p1, p2 ) w =
    S.polyline
        [ SA.points [ tupleFromVec p1, tupleFromVec p2 ]
        , SA.stroke <| Color.brown
        , SA.strokeWidth <| Num w
        , SA.strokeLinecap TypedSvg.Types.StrokeLinecapRound
        ]
        []


drawRefLink : ( Gear, Gear, Ref ) -> List (Svg msg)
drawRefLink ( g, gg, ref ) =
    let
        ( minG, maxG ) =
            if (Fract.toFloat <| Gear.getFract g) < (Fract.toFloat <| Gear.getFract gg) then
                ( g, gg )

            else
                ( gg, g )

        centers =
            ( Gear.getPos minG, Gear.getPos maxG )

        dir =
            Vec.direction (Tuple.second centers) (Tuple.first centers)

        r =
            Gear.getLength ( minG, ref ) / 2

        w =
            r / 15

        exC =
            Vec.scale r <| vec2 -(Vec.getY dir) (Vec.getX dir)

        d1 =
            Vec.add exC

        d2 =
            Vec.add <| Vec.negate exC
    in
    [ drawLink (Tuple.mapBoth d1 d1 centers) w
    , drawLink (Tuple.mapBoth d2 d2 centers) w
    ]


tupleFromVec : Vec2 -> ( Float, Float )
tupleFromVec v =
    ( Vec.getX v, Vec.getY v )
