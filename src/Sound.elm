module Sound exposing
    ( Sound
    , decoder
    , fakeSound
    , getEngined
    , getLoopPercents
    , getName
    , length
    )

import Json.Decode as D
import Json.Decode.Field as Field
import Json.Encode as E


type Sound
    = S
        { path : String
        , duration : Float
        , startPercent : Float
        , endPercent : Float
        , loopPercent : Float
        }


fakeSound : Float -> Sound
fakeSound f =
    S
        { path = ""
        , duration = f
        , startPercent = 0
        , endPercent = 1
        , loopPercent = 0
        }


type alias Engined =
    { path : String
    , startPercent : Float
    , endPercent : Float
    , loopPercent : Float
    }


getEngined : Sound -> Engined
getEngined (S s) =
    { path = s.path
    , startPercent = s.startPercent
    , endPercent = s.endPercent
    , loopPercent = s.loopPercent
    }


length : Sound -> Float
length (S s) =
    s.duration * (s.endPercent - s.startPercent)


getPath : Sound -> String
getPath (S { path }) =
    path


getName : Sound -> String
getName (S { path }) =
    fileNameFromPath path


fileNameFromPath : String -> String
fileNameFromPath =
    Maybe.withDefault ""
        << List.head
        << String.split "."
        << Maybe.withDefault ""
        << List.head
        << List.reverse
        << String.split "/"


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


divide : Int -> Sound -> ( List Sound, List Float )
divide n (S s) =
    let
        durPercent =
            s.endPercent - s.startPercent

        mapPercent f =
            s.startPercent + f * durPercent

        nn =
            toFloat n

        fs =
            List.range 1 n
                |> List.map toFloat
                |> List.map (\i -> ( (i - 1) / nn, i / nn ))
                |> List.map (Tuple.mapBoth mapPercent mapPercent)

        divs =
            List.map Tuple.first <| List.drop 1 fs

        sounds =
            List.map (\tfs -> setLoop (Tuple.mapBoth Just Just tfs) <| S s) fs
    in
    ( sounds, divs )


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
                                    Field.attempt "loopPercent" D.float <|
                                        \mayLoop ->
                                            D.succeed <|
                                                let
                                                    start =
                                                        Maybe.withDefault 0 mayStart
                                                in
                                                S
                                                    { path = p
                                                    , duration = l
                                                    , startPercent = start
                                                    , endPercent = Maybe.withDefault 1 mayEnd
                                                    , loopPercent = Maybe.withDefault start mayLoop
                                                    }


encoder : Sound -> E.Value
encoder (S s) =
    E.object <|
        [ ( "path", E.string s.path )
        , ( "length", E.float s.duration )
        , ( "startPercent", E.float s.startPercent )
        , ( "endPercent", E.float s.endPercent )
        , ( "loopPercent", E.float s.loopPercent )
        ]
