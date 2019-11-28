module Sound exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline exposing (..)
import Json.Encode as E


type alias Sound =
    { path : String
    , length : Float
    }


noSound =
    { path = "NO_SOUND", length = 0 }


decoder : D.Value -> Result D.Error Sound
decoder v =
    D.decodeValue
        (D.succeed Sound
            |> required "path" D.string
            |> required "length" D.float
        )
        v


encoder : Sound -> E.Value
encoder s =
    E.object <|
        [ ( "path", E.string s.path )
        , ( "length", E.float s.length )
        ]
