module Sound exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline exposing (..)


type Sound
    = S SoundInternal


type alias SoundInternal =
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


decoder : D.Value -> Result D.Error Sound
decoder v =
    Result.map S <|
        D.decodeValue
            (D.succeed SoundInternal
                |> required "path" D.string
                |> required "length" D.float
            )
            v
