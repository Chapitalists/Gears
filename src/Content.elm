module Content exposing (..)

import Coll exposing (Coll, Id)
import Gear exposing (Gear)
import Sound exposing (Sound)


type Content item
    = M (Mobile item)
    | C (Collar item)
    | S Sound


type alias Mobile item =
    { motor : Id (Gear item), gears : Coll (Gear item) }


type alias Collar item =
    List { length : Float, wheel : item }
