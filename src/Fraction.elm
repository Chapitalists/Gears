module Fraction exposing (..)


type Fraction
    = F { num : Int, den : Int }


unit : Int -> Fraction
unit den =
    F { num = 1, den = den }


integer : Int -> Fraction
integer num =
    F { num = num, den = 1 }


toFloat : Fraction -> Float
toFloat (F { num, den }) =
    Basics.toFloat num / Basics.toFloat den


multiplication : Fraction -> Fraction -> Fraction
multiplication (F f1) (F f2) =
    F { num = f1.num * f2.num, den = f1.den * f2.den }
