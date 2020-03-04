module Sound exposing (..)

import Json.Decode as D
import Json.Decode.Field as Field
import Json.Encode as E


type Sound
    = S
        { path : String
        , duration : Float
        , startPercent : Float
        , endPercent : Float
        }



{- }
   fromPath : String -> Sound
   fromPath p =
       S { path = p }
-}


noSound =
    S { path = "NO_SOUND", duration = 0, startPercent = 0, endPercent = 0 }


length : Sound -> Float
length (S s) =
    s.duration * (s.endPercent - s.startPercent)


toString : Sound -> String
toString (S { path }) =
    path


getLoopPoints : Sound -> List Float
getLoopPoints (S { startPercent, endPercent, duration }) =
    [ startPercent * duration, endPercent * duration ]


getLoopPercents : Sound -> ( Float, Float )
getLoopPercents (S { startPercent, endPercent }) =
    ( startPercent, endPercent )


setLoop : ( Maybe Float, Maybe Float ) -> Sound -> Sound
setLoop mays (S s) =
    case mays of
        ( Just start, Nothing ) ->
            S { s | startPercent = clamp 0 s.endPercent start }

        ( Nothing, Just end ) ->
            S { s | endPercent = clamp s.startPercent 1 end }

        ( Just start, Just end ) ->
            let
                safeStart =
                    clamp 0 1 start
            in
            S { s | startPercent = safeStart, endPercent = clamp safeStart 1 end }

        _ ->
            S s


chgPath : Sound -> String -> Sound
chgPath (S s) p =
    S { s | path = p }


decoder : D.Decoder Sound
decoder =
    Field.require "path" D.string <|
        \p ->
            Field.require "length" D.float <|
                \l ->
                    Field.attempt "startPercent" D.float <|
                        \mayStart ->
                            Field.attempt "endPercent" D.float <|
                                \mayEnd ->
                                    D.succeed <|
                                        S
                                            { path = p
                                            , duration = l
                                            , startPercent = Maybe.withDefault 0 mayStart
                                            , endPercent = Maybe.withDefault 1 mayEnd
                                            }


encoder : Sound -> E.Value
encoder (S s) =
    E.object <|
        [ ( "path", E.string s.path )
        , ( "length", E.float s.duration )
        , ( "startPercent", E.float s.startPercent )
        , ( "endPercent", E.float s.endPercent )
        ]
