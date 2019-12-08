module Content exposing (..)

import Coll exposing (Coll, Id)
import Gear exposing (Gear)
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
            Debug.todo "Encode collar"


decoder : D.Decoder item -> item -> D.Decoder (Content item)
decoder wheelDecoder defaultWheel =
    D.oneOf
        [ Field.require "sound" Sound.decoder <| \sound -> D.succeed <| S sound
        , Field.require "mobile" (mobileDecoder wheelDecoder defaultWheel) <| \mobile -> D.succeed <| M mobile
        ]


type alias Mobile item =
    { motor : Id (Gear item), gears : Coll (Gear item) }


mobileEncoder : (item -> List ( String, E.Value )) -> Mobile item -> E.Value
mobileEncoder wheelEncoder m =
    E.object
        [ ( "motor", Coll.idEncoder m.motor )
        , ( "gears"
          , Coll.encoder m.gears <| Gear.encoder <| wheelEncoder
          )
        ]


mobileDecoder : D.Decoder item -> item -> D.Decoder (Mobile item)
mobileDecoder wheelDecoder defaultWheel =
    D.succeed Mobile
        |> required "motor" Coll.idDecoder
        |> required "gears"
            (Coll.decoder
                (Gear.decoder <| wheelDecoder)
                Gear.typeString
             <|
                Gear.default defaultWheel
            )


type alias Collar item =
    List { length : Float, wheel : item }
