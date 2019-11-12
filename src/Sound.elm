module Sound exposing (..)


type Sound
    = S { path : String }


fromPath : String -> Sound
fromPath p =
    S { path = p }


length : a -> Float
length s =
    1


toString : Sound -> String
toString (S { path }) =
    path
