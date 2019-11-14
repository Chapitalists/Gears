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
