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
    { matrice : Float, loop : Float, head : Bead item, beads : List (Bead item) }


getBeads : Collar item -> List (Bead item)
getBeads c =
    c.head :: c.beads


collarEncoder : (item -> List ( String, E.Value )) -> Collar item -> E.Value
collarEncoder wheelEncoder c =
    E.object
        [ ( "matriceLength", E.float c.matrice )
        , ( "loopStart", E.float c.loop )
        , ( "beads"
          , E.list (beadEncoder wheelEncoder) <| getBeads c
          )
        ]


collarDecoder : D.Decoder item -> D.Decoder (Collar item)
collarDecoder wheelDecoder =
    Field.require "matriceLength" D.float <|
        \matrice ->
            Field.require "loopStart" D.float <|
                \loop ->
                    Field.require "beads" (D.list <| beadDecoder wheelDecoder) <|
                        \beads ->
                            case beads of
                                head :: list ->
                                    D.succeed { matrice = matrice, loop = loop, head = head, beads = list }

                                _ ->
                                    D.fail "Collar should have at least one bead"
