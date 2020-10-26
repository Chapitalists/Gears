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


fileName : Sound -> String
fileName (S { path }) =
    Maybe.withDefault "" <|
        List.head <|
            String.split "." <|
                Maybe.withDefault "" <|
                    List.head <|
                        List.reverse <|
                            String.split "/" path


getLoopPercentsList : Sound -> List Float
getLoopPercentsList (S { startPercent, endPercent }) =
    [ startPercent, endPercent ]


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


division : Int -> Sound -> List Sound
division n (S s) =
    let
        durPercent =
            s.endPercent - s.startPercent

        loopPoints ( f1, f2 ) =
            ( Just <| s.startPercent + f1 * durPercent, Just <| s.startPercent + f2 * durPercent )

        nn =
            toFloat n
    in
    List.range 1 n
        |> List.map toFloat
        |> List.map (\i -> ( (i - 1) / nn, i / nn ))
        |> List.map (\fs -> setLoop (loopPoints fs) <| S s)


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
