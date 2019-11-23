module Fraction exposing (..)


type Fraction
    = F { num : Int, den : Int }


unit : Int -> Fraction
unit den =
    F { num = 1, den = den }


integer : Int -> Fraction
integer num =
    F { num = num, den = 1 }


fromRecord : { num : Int, den : Int } -> Fraction
fromRecord =
    F


toFloat : Fraction -> Float
toFloat (F { num, den }) =
    Basics.toFloat num / Basics.toFloat den


getNumerator : Fraction -> Int
getNumerator (F { num }) =
    num


getDenominator : Fraction -> Int
getDenominator (F { den }) =
    den


multiplication : Fraction -> Fraction -> Fraction
multiplication (F f1) (F f2) =
    F { num = f1.num * f2.num, den = f1.den * f2.den }


division : Fraction -> Fraction -> Fraction
division (F num) (F den) =
    F { num = num.num * den.den, den = num.den * den.num }
