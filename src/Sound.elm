module Sound exposing (..)

import Json.Decode as D
import Json.Decode.Field as Field
import Json.Encode as E


type Sound
    = S
        { path : String
        , length : Float
        }



{- }
   fromPath : String -> Sound
   fromPath p =
       S { path = p }
-}


noSound =
    S { path = "NO_SOUND", length = 0 }


length : Sound -> Float
length (S s) =
    s.length


toString : Sound -> String
toString (S { path }) =
    path


chgPath : Sound -> String -> Sound
chgPath (S s) p =
    S { s | path = p }


decoder : D.Decoder Sound
decoder =
    Field.require "path" D.string <|
        \p ->
            Field.require "length" D.float <|
                \l ->
                    D.succeed <|
                        S
                            { path = p
                            , length = l
                            }


encoder : Sound -> E.Value
encoder (S s) =
    E.object <|
        [ ( "path", E.string s.path )
        , ( "length", E.float s.length )
        ]
