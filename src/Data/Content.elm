module Data.Content exposing (..)

import Coll exposing (Coll, Id)
import Data.Gear as Gear exposing (Gear)
import Json.Decode as D
import Json.Decode.Field as Field
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Sound exposing (Sound)


type Content item
    = M (Mobile item)
    | C (Collar item)
    | S Sound


encoder : (item -> List ( String, E.Value )) -> Content item -> ( String, E.Value )
encoder wheelEncoder content =
    case content of
        S s ->
            ( "sound", Sound.encoder s )

        M m ->
            ( "mobile", mobileEncoder wheelEncoder m )

        C c ->
            ( "collar", collarEncoder wheelEncoder c )


decoder : D.Decoder item -> item -> D.Decoder (Content item)
decoder wheelDecoder defaultWheel =
    D.oneOf
        [ Field.require "sound" Sound.decoder <| \sound -> D.succeed <| S sound
        , Field.require "mobile" (mobileDecoder wheelDecoder defaultWheel) <| \mobile -> D.succeed <| M mobile
        , Field.require "collar" (collarDecoder wheelDecoder) <| \collar -> D.succeed <| C collar
        ]


type alias Mobile item =
    { motor : Id (Gear item), gears : Coll (Gear item) }


getGear : Id (Gear item) -> Mobile item -> Gear item
getGear id m =
    Coll.get id m.gears


updateGear : Id (Gear item) -> (Gear item -> Gear item) -> Mobile item -> Mobile item
updateGear id f m =
    { m | gears = Coll.update id f m.gears }


mobileEncoder : (item -> List ( String, E.Value )) -> Mobile item -> E.Value
mobileEncoder wheelEncoder m =
    E.object
        [ ( "motor", Coll.idEncoder m.motor )
        , ( "gears"
          , Coll.encoder m.gears <| Gear.encoder wheelEncoder
          )
        ]


mobileDecoder : D.Decoder item -> item -> D.Decoder (Mobile item)
mobileDecoder wheelDecoder defaultWheel =
    D.succeed Mobile
        |> required "motor" Coll.idDecoder
        |> required "gears"
            (Coll.decoder
                (Gear.decoder wheelDecoder)
                Gear.typeString
             <|
                Gear.default defaultWheel
            )


beadUIDExtension : String -> Int -> String
beadUIDExtension parentUid i =
    parentUid ++ "-" ++ String.fromInt i


type alias Bead item =
    { length : Float, wheel : item }


beadEncoder : (item -> List ( String, E.Value )) -> Bead item -> E.Value
beadEncoder wheelEncoder b =
    E.object <| ( "length", E.float b.length ) :: wheelEncoder b.wheel


beadDecoder : D.Decoder item -> D.Decoder (Bead item)
beadDecoder wheelDecoder =
    wheelDecoder
        |> D.andThen
            (\w ->
                Field.require "length" D.float <|
                    \l ->
                        D.succeed { length = l, wheel = w }
            )


type alias Collar item =
    { matrice : Int, loop : Float, head : Bead item, beads : List (Bead item) }


getBeads : Collar item -> List (Bead item)
getBeads c =
    c.head :: c.beads



-- TODO if i >= length, get differs from update, bug, see Common.deleteWheel, updates the got bead


getBead : Int -> Collar item -> Bead item
getBead i c =
    case List.head <| List.drop i <| getBeads c of
        Just b ->
            b

        Nothing ->
            Debug.log ("Cannot get Bead " ++ String.fromInt i) <| c.head


updateBead : Int -> (Bead item -> Bead item) -> Collar item -> Collar item
updateBead i f c =
    if i <= 0 then
        { c | head = f c.head }

    else
        { c
            | beads =
                List.concat
                    [ List.take (i - 1) c.beads
                    , case List.head <| List.drop (i - 1) c.beads of
                        Nothing ->
                            []

                        Just b ->
                            [ f b ]
                    , List.drop i c.beads
                    ]
        }


getCumulLengthAt : Int -> Collar item -> Float
getCumulLengthAt i c =
    List.foldl (\b sum -> sum + b.length) 0 <| List.take i <| getBeads c


getMatriceLength : Collar item -> Float
getMatriceLength c =
    getCumulLengthAt c.matrice c


collarEncoder : (item -> List ( String, E.Value )) -> Collar item -> E.Value
collarEncoder wheelEncoder c =
    E.object
        [ ( "matriceSize", E.int c.matrice )
        , ( "loopStart", E.float c.loop )
        , ( "beads"
          , E.list (beadEncoder wheelEncoder) <| getBeads c
          )
        ]


collarDecoder : D.Decoder item -> D.Decoder (Collar item)
collarDecoder wheelDecoder =
    Field.attempt "matriceSize" D.int <|
        \mayMatrice ->
            Field.require "loopStart" D.float <|
                \loop ->
                    Field.require "beads" (D.list <| beadDecoder wheelDecoder) <|
                        \beads ->
                            let
                                matrice =
                                    Maybe.withDefault (List.length beads) mayMatrice
                            in
                            case beads of
                                head :: list ->
                                    D.succeed { matrice = matrice, loop = loop, head = head, beads = list }

                                _ ->
                                    D.fail "Collar should have at least one bead"
