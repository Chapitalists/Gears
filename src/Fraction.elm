module Fraction exposing (..)

import Json.Decode as D
import Json.Encode as E


type alias Fraction =
    { num : Int, den : Int }


unit : Int -> Fraction
unit den =
    { num = 1, den = den }


integer : Int -> Fraction
integer num =
    { num = num, den = 1 }


toFloat : Fraction -> Float
toFloat { num, den } =
    Basics.toFloat num / Basics.toFloat den


toString : Fraction -> String
toString { num, den } =
    String.fromInt num ++ " / " ++ String.fromInt den


multiplication : Fraction -> Fraction -> Fraction
multiplication f1 f2 =
    { num = f1.num * f2.num, den = f1.den * f2.den }


division : Fraction -> Fraction -> Fraction
division num den =
    { num = num.num * den.den, den = num.den * den.num }


simplify : Fraction -> Fraction
simplify { num, den } =
    let
        common =
            pgcd num den
    in
    { num = num // common, den = den // common }


pgcd : Int -> Int -> Int
pgcd x y =
    if x >= y then
        let
            rest =
                modBy y x
        in
        if rest == 0 then
            y

        else
            pgcd y rest

    else
        pgcd y x


encoder : Fraction -> E.Value
encoder f =
    E.object <|
        [ ( "num", E.int f.num )
        , ( "den", E.int f.den )
        ]


decoder : D.Decoder Fraction
decoder =
    D.map2 Fraction (D.field "num" D.int) (D.field "den" D.int)
